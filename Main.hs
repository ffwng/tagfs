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


data Status = Status { getRoute :: Route FileEntry, getTagSet :: TagSet }

-- helper functions

returnLeft = return . Left

returnRight = return . Right

getEntryStat :: FuseContext -> FilePath -> FileEntry -> IO FileStat
getEntryStat _ basedir (RegularFile name) = realFileStat $ basedir </> name
getEntryStat ctx _ (TagFile tags _) = return $ fileStat ctx (tagFileContentLength tags)

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
		Just (Right e) -> Right <$> getEntryStat ctx basedir e
		Just (Left dir) -> returnRight $ dirStat ctx

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
		Just (Right _) -> returnLeft eNOTDIR
		Just (Left (Dir dirs files)) -> do
			fileStats <- zip (map getPath files) <$> mapM (getEntryStat ctx basedir) files
			let dirStats = map (,dirStat ctx) dirs
			returnRight $ defaultStats ctx ++ dirStats ++ fileStats

createDirectory :: FilePath -> IORef Status -> FilePath -> FileMode -> IO Errno
createDirectory basedir ref (_:p) _ = do
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


-- files

tempFile :: IO Fd
tempFile = do
	(path, handle) <- mkstemps "" ""
	removeLink path
	handleToFd handle



-- filesystem

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats str =  returnRight FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5 -- IS THIS CORRECT?
    , fsStatFilesFree = 10 -- WHAT IS THIS?
    , fsStatMaxNameLength = 255 -- SEEMS SMALL?
    }


fsOps :: FilePath -> IORef Status -> FuseOperations Fd
fsOps basedir r = defaultFuseOps
	{ fuseGetFileStat = getFileStat basedir r
	, fuseOpenDirectory = openDirectory basedir r
	, fuseReadDirectory = readDirectory basedir r
	, fuseCreateDirectory = createDirectory basedir r
	, fuseGetFileSystemStats = getFileSystemStats
	}

main = do
	let ts = fromFiles ["boo", "bar", "baz"]
		[("file1", ["bar"]), ("file2", []), ("file3", [])]
	let basedir = "/tmp"
	-- ^ for testing purposes only
	let route = buildBaseRoute ts
	status <- newIORef $ Status route ts
	fuseMain (fsOps basedir status) defaultExceptionHandler
