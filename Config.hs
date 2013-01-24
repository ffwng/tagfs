{-# LANGUAGE OverloadedStrings #-}
module Config where

import Control.Monad
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import qualified Data.Text as T
import Control.Arrow

import TagSet (Tag(..))

toRepr :: (FilePath, Tag) -> (T.Text, String, Maybe String)
toRepr (p, t) = (T.pack p, n, v) where (n, v) = tagToRepr t

fromRepr :: (T.Text, String, Maybe String) -> (FilePath, Tag)
fromRepr (t, n, v) = (T.unpack t, tag) where tag = tagFromRepr (n, v)

tagToRepr :: Tag -> (String, Maybe String)
tagToRepr (Simple n) = (n, Nothing)
tagToRepr (Extended n v) = (n, Just v)

tagFromRepr :: (String, Maybe String) -> Tag
tagFromRepr (n, Nothing) = Simple n
tagFromRepr (n, Just v) = Extended n v

data Config = Config { mapping :: [(FilePath, Tag)], tags :: [Tag] }

createEmptyTable :: Connection -> IO ()
createEmptyTable c = do
	execute_ c "DROP TABLE IF EXISTS files"
	execute_ c
		"CREATE TABLE files (id INTEGER PRIMARY KEY, path TEXT, name TEXT, value TEXT)"
	execute_ c "CREATE TABLE tags (id INTEGER PRIMARY KEY, name TEXT, value TEXT)"

readConfig :: FilePath -> IO Config
readConfig s = withConnection s $ \c -> do
	m <- map fromRepr <$> query_ c "SELECT path, name, value FROM files"
	t <- map tagFromRepr <$> query_ c "SELECT name, value FROM tags"
	return $ Config m t

writeConfig :: Config -> FilePath -> IO ()
writeConfig (Config fs ts) s = withConnection s $ \c -> do
	createEmptyTable c
	forM_ fs $ \f -> execute c
		"INSERT INTO files (path, name, value) VALUES (?, ?, ?)" (toRepr f)
	forM_ ts $ \t -> execute c
		"INSERT INTO tags (name, value) VALUES (?, ?)" (tagToRepr t)
