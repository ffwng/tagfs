module Main where

import Control.Monad
import Control.Arrow
import System.Directory
import Data.IORef
import System.Fuse (fuseMain, defaultExceptionHandler)
import System.FilePath
import Data.Maybe
import System.Environment
import Data.Functor

import FuseOperations
import TagFS (TagSet)
import Config

{-ts :: TagSet
ts = fromFiles [Simple "bar", Extended "loc" "hier", Extended "loc" "da"]
	[("file1", [Simple "bar"]), ("file2", [Extended "loc" "hier"]), ("file3", [])]

mapping :: M.Map FilePath FilePath
mapping = M.fromList [("file1", "/tmp/file1"), ("file2", "/tmp/file2"),
	("file3", "/tmp/file3")]-}
-- ^ for testing purposes only

getFiles :: FilePath -> IO [(FilePath, FilePath)]
getFiles p = filterM (doesFileExist . fst) . map (id &&& (p </>)) =<< getDirectoryContents p

interpretArg :: String -> IO [(FilePath, FilePath)]
interpretArg s = do
	path <- canonicalizePath s
	b <- doesDirectoryExist path
	if b then getFiles path
	else do
		let s' = takeFileName s
		return [(s', path)]

configPath :: FilePath
configPath = "tagfs.conf"

saveConfig :: FilePath -> Config -> TagSet -> IO ()
saveConfig p c ts = do
	let c' = c { tagSet = ts }
	writeConfig p c'

main :: IO ()
main = do
	conf <- fromMaybe emptyConfig <$> readConfig configPath
	args <- getArgs
	case args of
		"mount":xs -> withArgs xs $ do
			path <- canonicalizePath configPath
			status <- newIORef $ newStatus (tagSet conf) (mapping conf)
				(saveConfig path conf)
			fuseMain (fsOps status) defaultExceptionHandler
		"add":xs -> do
			files <- concat <$> mapM interpretArg xs
			let conf' = foldr Config.addFile conf files
			writeConfig configPath conf'
		"remove":xs -> do
			let conf' = foldr Config.removeFile conf xs
			writeConfig configPath conf'
		"tag":t:xs -> case parseTag t of
			Just t' -> do
				let conf' = foldr (Config.tagFile t') conf xs
				writeConfig configPath conf'
			_ -> return ()
		_ -> putStrLn "could not interpret argument"
