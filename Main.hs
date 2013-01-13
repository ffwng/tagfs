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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe


data Status = Status
	{ getRoute :: Route Entry
	, getTagSet :: TagSet
	, getFileMapping :: Map FilePath FilePath
	}

getRealPath :: Status -> FilePath -> FilePath
getRealPath s p = fromMaybe "/dev/null" $ M.lookup p (getFileMapping s)

newStatus :: TagSet -> Map FilePath FilePath -> Status
newStatus ts m = Status (buildBaseRoute ts) ts m

updateStatus :: Status -> TagSet -> Status
updateStatus s ts = s { getRoute = buildBaseRoute ts, getTagSet = ts }

-- helper functions

returnLeft = return . Left

returnRight = return . Right

getEntryStat :: Status -> FuseContext -> Entry -> IO FileStat
getEntryStat s _ (RegularFile name) = realFileStat $ getRealPath s name
getEntryStat _ ctx (TagFile tags _ _) = return $ fileStat ctx (tagFileContentLength tags)
getEntryStat _ ctx e | isDir e = return $ dirStat ctx
getEntryStat _ _ _ = error "getEntryStat"

tagFileContent :: [Tag] -> ByteString
tagFileContent = B.pack . unlines

tagFileContentLength :: [Tag] -> Int
tagFileContentLength = B.length . tagFileContent

parseTags :: ByteString -> [Tag]
parseTags = lines . B.unpack

-- fuse operations

getFileStat :: IORef Status -> FilePath -> IO (Either Errno FileStat)
getFileStat ref p = do
	ctx <- getFuseContext
	status <- readIORef ref
	let r = getRoute status
	case route r p of
		Nothing -> returnLeft eNOENT
		Just e -> Right <$> getEntryStat status ctx e

-- directories

defaultStats ctx = [(".", dirStat ctx), ("..", dirStat ctx)]

openDirectory :: IORef Status -> FilePath -> IO Errno
openDirectory _ _ = return eOK

readDirectory :: IORef Status -> FilePath
	-> IO (Either Errno [(FilePath, FileStat)])
readDirectory ref p = do
	ctx <- getFuseContext
	status <- readIORef ref
	r <- getRoute <$> readIORef ref
	case route r p of
		Nothing -> returnLeft eNOENT
		Just dir -> case getDirEntries dir of
			Nothing -> returnLeft eNOTDIR
			Just entries -> do
				stats <- zip (map getPath entries)
					<$> mapM (getEntryStat status ctx) entries
				returnRight $ defaultStats ctx ++ stats

createDirectory :: IORef Status -> FilePath -> FileMode -> IO Errno
createDirectory ref (_:p) _ = do
	let seg = splitDirectories p
	status <- readIORef ref
	let r = getRoute status
	case runRoute r seg of
		Just _ -> return eEXIST
		_ -> do
			let name = last seg
			let ts = getTagSet status
			let tsNew = createTag name ts
			writeIORef ref (updateStatus status tsNew)
			return eOK

removeDirectory :: IORef Status -> FilePath -> IO Errno
removeDirectory ref (_:p) = do
	let seg = splitDirectories p
	status <- readIORef ref
	let r = getRoute status
	case runRoute r seg of
		Nothing -> return eNOENT
		Just (TagDir _ _ _) -> do
			let name = last seg
			let ts = getTagSet status
			let tsNew = wipeTag name ts
			writeIORef ref (updateStatus status tsNew)
			return eOK
		_ -> return eNOTDIR

-- files

tempFile :: IO Handle
tempFile = do
	(path, handle) <- openTempFile "/tmp" ""
	removeLink path
	return handle

tagfsOpen ::  IORef Status -> FilePath -> OpenMode -> OpenFileFlags
	-> IO (Either Errno Handle)
tagfsOpen ref p mode flags = do
	status <- readIORef ref
	let r = getRoute status
	case route r p of
		Nothing -> returnLeft eNOENT
		Just (RegularFile name) -> do
			fd <- openFd (getRealPath status name) mode Nothing flags
			h <- fdToHandle fd
			returnRight h
		Just (TagFile t _ _) -> do
			h <- tempFile
			B.hPut h (tagFileContent t)
			returnRight h
		_ -> returnLeft ePERM

tagfsRead :: IORef Status -> FilePath -> Handle -> ByteCount -> FileOffset
	-> IO (Either Errno ByteString)
tagfsRead _ _ h count offset = do
	hSeek h AbsoluteSeek (toInteger offset)
	Right <$> B.hGet h (fromInteger $ toInteger count)

tagfsWrite :: IORef Status -> FilePath -> Handle -> ByteString -> FileOffset
	-> IO (Either Errno ByteCount)
tagfsWrite _ _ h content offset = do
	hSeek h AbsoluteSeek (toInteger offset)
	B.hPut h content
	returnRight . fromInteger . toInteger $ B.length content

tagfsRelease :: IORef Status -> FilePath -> Handle -> IO ()
tagfsRelease ref p h = do
	status <- readIORef ref
	let r = getRoute status
	case route r p of
		Just (TagFile _ name _) -> do
			hSeek h AbsoluteSeek 0
			content <- B.hGetContents h
			let ts = getTagSet status
			let tsNew = setTags (parseTags content) name ts
			writeIORef ref (updateStatus status tsNew)
		_ -> return ()
	hClose h

tagfsSetFileSize :: IORef Status -> FilePath -> FileOffset -> IO Errno
tagfsSetFileSize ref p size = do
	status <- readIORef ref
	let r = getRoute status
	case route r p of
		Nothing -> return eNOENT
		Just (RegularFile name) -> setFileSize (getRealPath status name) size >> return eOK
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

fsOps :: IORef Status -> FuseOperations Handle
fsOps r = defaultFuseOps
	{ fuseGetFileStat = getFileStat r
	, fuseOpenDirectory = openDirectory r
	, fuseReadDirectory = readDirectory r
	, fuseCreateDirectory = createDirectory r
	, fuseRemoveDirectory = removeDirectory r
	, fuseOpen = tagfsOpen r
	, fuseRead = tagfsRead r
	, fuseWrite = tagfsWrite r
	, fuseRelease = tagfsRelease r
	, fuseSetFileSize = tagfsSetFileSize r
	, fuseGetFileSystemStats = getFileSystemStats
	}
	
ts = fromFiles ["boo", "bar", "baz"]
	[("file1", ["bar"]), ("file2", []), ("file3", [])]
mapping = M.fromList [("file1", "/tmp/file1"), ("file2", "/tmp/file2"),
	("file3", "/tmp/file3")]
-- ^ for testing purposes only

main = do
	status <- newIORef $ newStatus ts mapping
	fuseMain (fsOps status) defaultExceptionHandler
