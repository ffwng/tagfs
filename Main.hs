module Main where

import Control.Monad
import Control.Arrow
import System.Directory
import Data.IORef
import System.Fuse (fuseMain, defaultExceptionHandler)
import System.FilePath
import qualified Data.Map as M

import TagSet hiding (TagSet)
import TagFS
import FuseOperations

ts = fromFiles [Simple "bar", Extended "loc" "hier", Extended "loc" "da"]
	[("file1", [Simple "bar"]), ("file2", [Extended "loc" "hier"]), ("file3", [])]
mapping = M.fromList [("file1", "/tmp/file1"), ("file2", "/tmp/file2"),
	("file3", "/tmp/file3")]
-- ^ for testing purposes only

getFiles :: FilePath -> IO [(FilePath, FilePath)]
getFiles p = filterM (doesFileExist . fst) . map ((p </>) &&& id) =<< getDirectoryContents p

main = do
	status <- newIORef $ newStatus ts mapping
	fuseMain (fsOps status) defaultExceptionHandler
