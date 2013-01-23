{-# LANGUAGE OverloadedStrings #-}
module Config where

import Control.Monad
import Control.Applicative
import Database.SQLite.Simple
import qualified Data.Text as T

data Config = Config { files :: [FilePath] }

createEmptyTable :: Connection -> IO ()
createEmptyTable c = do
	execute_ c "DROP TABLE IF EXISTS files"
	execute_ c "CREATE TABLE files (id INTEGER PRIMARY KEY, path TEXT)"

readConfig :: FilePath -> IO Config
readConfig s = withConnection s $ \c ->
	Config . map (T.unpack . fromOnly) <$> query_ c "SELECT path FROM files"

writeConfig :: Config -> FilePath -> IO ()
writeConfig (Config fs) s = withConnection s $ \c -> do
	createEmptyTable c
	forM_ fs $ \f -> execute c "INSERT INTO files (path) VALUES (?)" (Only f)
