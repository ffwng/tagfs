{-# LANGUAGE TupleSections #-}
module Main where

{-- semantics:

readdir - filter
link/sysmlink - tag assignment
unlink - tag deletion
mkdir - new tag
read - read

--}


import TagSet
import Route
import TagFS
import Stat

import System.IO
import Control.Applicative
import Data.IORef
import System.Fuse hiding (RegularFile)
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.FilePath
import Control.Arrow (second)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import System.Posix.Temp


data Status = Status { getRoute :: Route Entry, getTagSet :: TagSet }

-- helper functions

returnLeft = return . Left

returnRight = return . Right

getEntryStat :: FuseContext -> FilePath -> Entry -> IO FileStat
getEntryStat _ basedir (RegularFile name) = realFileStat $ basedir </> name
getEntryStat ctx _ (TagFile tags _ _) = return $ fileStat ctx (tagFileContentLength tags)
getEntryStat ctx _ e | isDir e = return $ dirStat ctx
getEntryStat _ _ _ = error "getEntryStat"

tagFileContent :: [Tag] -> ByteString
tagFileContent = B.pack . unlines

tagFileContentLength :: [Tag] -> Int
tagFileContentLength = B.length . tagFileContent

parseTags :: ByteString -> [Tag]
parseTags = lines . B.unpack

-- fuse operations

getFileStat :: FilePath -> IORef Status -> FilePath -> IO (Either Errno FileStat)
getFileStat basedir ref p = do
	ctx <- getFuseContext
	r <- getRoute <$> readIORef ref
	case route r p of
		Nothing -> returnLeft eNOENT
		Just e -> Right <$> getEntryStat ctx basedir e

-- directories

defaultStats ctx = [(".", dirStat ctx), ("..", dirStat ctx)]

openDirectory :: FilePath -> IORef Status -> FilePath -> IO Errno
openDirectory _ _ _ = return eOK

readDirectory :: FilePath -> IORef Status -> FilePath
	-> IO (Either Errno [(FilePath, FileStat)])
readDirectory basedir ref p = do
	ctx <- getFuseContext
	r <- getRoute <$> readIORef ref
	case route r p of
		Nothing -> returnLeft eNOENT
		Just dir -> case getDirEntries dir of
			Nothing -> returnLeft eNOTDIR
			Just entries -> do
				stats <- zip (map getPath entries)
					<$> mapM (getEntryStat ctx basedir) entries
				returnRight $ defaultStats ctx ++ stats

createDirectory :: FilePath -> IORef Status -> FilePath -> FileMode -> IO Errno
createDirectory _ ref (_:p) _ = do
	let seg = splitDirectories p
	status <- readIORef ref
	let r = getRoute status
	case runRoute r seg of
		Just _ -> return eEXIST
		_ -> do
			let name = last seg
			let ts = getTagSet status
			let tsNew = createTag name ts
			let rNew = buildBaseRoute tsNew
			writeIORef ref (Status rNew tsNew)
			return eOK

removeDirectory :: FilePath -> IORef Status -> FilePath -> IO Errno
removeDirectory _ ref (_:p) = do
	let seg = splitDirectories p
	status <- readIORef ref
	let r = getRoute status
	case runRoute r seg of
		Nothing -> return eNOENT
		Just (TagDir _ _ _) -> do
			let name = last seg
			let ts = getTagSet status
			let tsNew = wipeTag name ts
			let rNew = buildBaseRoute tsNew
			writeIORef ref (Status rNew tsNew)
			return eOK
		_ -> return eNOTDIR

-- files

tempFile :: IO Handle
tempFile = do
	(path, handle) <- openTempFile "/tmp" ""
	removeLink path
	return handle

tagfsOpen :: FilePath -> IORef Status -> FilePath -> OpenMode -> OpenFileFlags
	-> IO (Either Errno Handle)
tagfsOpen basedir ref p mode flags = do
	r <- getRoute <$> readIORef ref
	case route r p of
		Nothing -> returnLeft eNOENT
		Just (RegularFile name) -> do
			fd <- openFd (basedir </> name) mode Nothing flags
			h <- fdToHandle fd
			returnRight h
		Just (TagFile t _ _) -> do
			h <- tempFile
			B.hPut h (tagFileContent t)
			returnRight h
		_ -> returnLeft ePERM

tagfsRead :: FilePath -> IORef Status -> FilePath -> Handle -> ByteCount -> FileOffset
	-> IO (Either Errno ByteString)
tagfsRead _ _ _ h count offset = do
	hSeek h AbsoluteSeek (toInteger offset)
	Right <$> B.hGet h (fromInteger $ toInteger count)

tagfsWrite :: FilePath -> IORef Status -> FilePath -> Handle -> ByteString -> FileOffset
	-> IO (Either Errno ByteCount)
tagfsWrite _ _ _ h content offset = do
	hSeek h AbsoluteSeek (toInteger offset)
	B.hPut h content
	returnRight . fromInteger . toInteger $ B.length content

tagfsRelease :: FilePath -> IORef Status -> FilePath -> Handle -> IO ()
tagfsRelease _ ref p h = do
	status <- readIORef ref
	let r = getRoute status
	case route r p of
		Just (TagFile _ name _) -> do
			hSeek h AbsoluteSeek 0
			content <- B.hGetContents h
			let ts = getTagSet status
			let tsNew = setTags (parseTags content) name ts
			let rNew = buildBaseRoute tsNew
			writeIORef ref (Status rNew tsNew)
		_ -> return ()
	hClose h

tagfsSetFileSize :: FilePath -> IORef Status -> FilePath -> FileOffset -> IO Errno
tagfsSetFileSize basedir ref p size = do
	r <- getRoute <$> readIORef ref
	case route r p of
		Nothing -> return eNOENT
		Just (RegularFile name) -> setFileSize (basedir </> name) size >> return eOK
		Just (TagFile _ _ _) -> return eOK
		_ -> return ePERM


-- filesystem

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats str =  returnRight FileSystemStats
	{ fsStatBlockSize = 510
	, fsStatBlockCount = 1
	, fsStatBlocksFree = 1
	, fsStatBlocksAvailable = 1
	, fsStatFileCount = 5 -- IS THIS CORRECT?
	, fsStatFilesFree = 10 -- WHAT IS THIS?
	, fsStatMaxNameLength = 255 -- SEEMS SMALL?
	}

fsOps :: FilePath -> IORef Status -> FuseOperations Handle
fsOps basedir r = defaultFuseOps
	{ fuseGetFileStat = getFileStat basedir r
	, fuseOpenDirectory = openDirectory basedir r
	, fuseReadDirectory = readDirectory basedir r
	, fuseCreateDirectory = createDirectory basedir r
	, fuseRemoveDirectory = removeDirectory basedir r
	, fuseOpen = tagfsOpen basedir r
	, fuseRead = tagfsRead basedir r
	, fuseWrite = tagfsWrite basedir r
	, fuseRelease = tagfsRelease basedir r
	, fuseSetFileSize = tagfsSetFileSize basedir r
	, fuseGetFileSystemStats = getFileSystemStats
	}
	
ts = fromFiles ["boo", "bar", "baz"]
	[("file1", ["bar"]), ("file2", []), ("file3", [])]
basedir = "/tmp"
baseroute = buildBaseRoute ts
-- ^ for testing purposes only

main = do
	status <- newIORef $ Status baseroute ts
	fuseMain (fsOps basedir status) defaultExceptionHandler
