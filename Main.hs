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
import System.FilePath
import Control.Arrow (second)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List

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

getFileStat :: IORef (Route FileEntry) -> FilePath -> FilePath -> IO (Either Errno FileStat)
getFileStat ref basedir p = do
	ctx <- getFuseContext
	r <- readIORef ref
	case route r p of
		Nothing -> returnLeft eNOENT
		Just (Right e) -> Right <$> getEntryStat ctx basedir e
		Just (Left dir) -> returnRight $ dirStat ctx

-- directories

defaultStats ctx = [(".", dirStat ctx), ("..", dirStat ctx)]

openDirectory :: IORef (Route FileEntry) -> FilePath -> FilePath -> IO Errno
openDirectory _ _ _ = return eOK

readDirectory :: IORef (Route FileEntry) -> FilePath -> FilePath
	-> IO (Either Errno [(FilePath, FileStat)])
readDirectory ref basedir p = do
	ctx <- getFuseContext
	r <- readIORef ref
	case route r p of
		Nothing -> returnLeft eNOENT
		Just (Right _) -> returnLeft eNOTDIR
		Just (Left (Dir dirs files)) -> do
			fileStats <- zip (map getPath files) <$> mapM (getEntryStat ctx basedir) files
			let dirStats = map (,dirStat ctx) dirs
			returnRight $ defaultStats ctx ++ dirStats ++ fileStats

-- files

tempFile :: IO Fd
tempFile = do



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


fsOps :: IORef (Route FileEntry) -> FilePath -> FuseOperations Fd
fsOps r basedir = defaultFuseOps
	{ fuseGetFileStat = getFileStat r basedir
	, fuseOpenDirectory = openDirectory r basedir
	, fuseReadDirectory = readDirectory r basedir
	, fuseGetFileSystemStats = getFileSystemStats
	}

main = do
	let files = fromFiles ["boo", "bar", "baz"]
		[("file1", ["bar"]), ("file2", []), ("file3", [])]
	let basedir = "/tmp"
	-- ^ for testing purposes only
	route <- newIORef (buildBaseRoute files)
	fuseMain (fsOps route basedir) defaultExceptionHandler
