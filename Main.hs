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

-- fuse operations

getFileStat :: IORef (Route FileEntry) -> FilePath -> IO (Either Errno FileStat)
getFileStat ref (p:ps) = do
	let seg = splitDirectories ps
	ctx <- getFuseContext
	r <- readIORef ref
	case runRoute r seg of
		Just (Right (RegularFile name)) -> Right <$> realFileStat ("/tmp" </> name)
		Just (Right (TagFile tags name)) -> return . Right $ fileStat ctx 1
		Just (Left _) -> return . Right $ dirStat ctx
		Nothing -> return $ Left eNOENT

openDirectory :: IORef (Route FileEntry) -> FilePath -> IO Errno
openDirectory _ _ = return eOK

readDirectory :: IORef (Route FileEntry) -> FilePath
	-> IO (Either Errno [(FilePath, FileStat)])
readDirectory ref (p:ps) = do
	let seg = splitDirectories ps
	ctx <- getFuseContext
	r <- readIORef ref
	case runRoute r seg of
		Nothing -> return $ Left eNOENT
		Just (Right _) -> return $ Left eNOTDIR
		Just (Left dir) -> case getSegments dir of
			Nothing -> return $ Left eNOTDIR
			Just paths -> do
				let def = [(".", dirStat ctx), ("..", dirStat ctx)]
				p <- mapM (findStat ctx) paths
				return . Right $ def ++ p
	where
		findStat ctx (p, r) = do
			r' <- case getLeaf r of
				Just (RegularFile name) -> realFileStat ("/tmp" </> name)
				Just (TagFile tags name) -> return $ fileStat ctx 1
				Nothing -> return $ dirStat ctx
			return (p, r')

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats str =  return $ Right FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5 -- IS THIS CORRECT?
    , fsStatFilesFree = 10 -- WHAT IS THIS?
    , fsStatMaxNameLength = 255 -- SEEMS SMALL?
    }


fsOps :: IORef (Route FileEntry) -> FuseOperations Fd
fsOps r = defaultFuseOps
	{ fuseGetFileStat = getFileStat r
	, fuseOpenDirectory = openDirectory r
	, fuseReadDirectory = readDirectory r
	, fuseGetFileSystemStats = getFileSystemStats
	}

main = do
	let files = fromFiles ["a", "b", "c"]
		[("file1", ["a"]), ("file2", []), ("file3", [])]
	-- ^ for testing purposes only
	route <- newIORef (buildBaseRoute files)
	fuseMain (fsOps route) defaultExceptionHandler
