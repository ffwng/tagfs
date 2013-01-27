{-# LANGUAGE OverloadedStrings #-}
module Config where

import Control.Monad
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import qualified Data.Text as T
import Control.Arrow
import Data.List
import Data.Maybe

import GHC.Exts (fromString)

import TagSet (Tag(..))

tagToRepr :: Int -> Tag -> (Int, String, Maybe String)
tagToRepr i (Simple n) = (i, n, Nothing)
tagToRepr i (Extended n v) = (i, n, Just v)

tagFromRepr :: (Int, String, Maybe String) -> Tag
tagFromRepr (_, n, Nothing) = Simple n
tagFromRepr (_, n, Just v) = Extended n v

fileToRepr :: Int -> FilePath -> (Int, FilePath)
fileToRepr = (,)

fileFromRepr :: (Int, FilePath) -> FilePath
fileFromRepr = snd

data Config = Config { tags :: [Tag], files :: [FilePath], mapping :: [(FilePath, Tag)] }
	deriving (Show)

createEmptyTable :: Connection -> String -> String -> IO ()
createEmptyTable c name rows = do
	execute_ c . fromString $ "DROP TABLE IF EXISTS " ++ name
	execute_ c . fromString $ "CREATE TABLE " ++ name ++ " (" ++ rows ++ ")"

createEmptyTables :: Connection -> IO ()
createEmptyTables c = do
	createEmptyTable c "tags" "id INTEGER PRIMARY KEY, name TEXT, value TEXT"
	createEmptyTable c "files" "id INTEGER PRIMARY KEY, path TEXT"
	createEmptyTable c "mapping" "fileid INTEGER, tagid INTEGER"

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (x:xs) n = safeIndex xs $! (n-1)

findElems :: [Tag] -> [FilePath] -> [(Int, Int)] -> [(FilePath, Tag)]
findElems ts fs = mapMaybe helper where
	helper (i1, i2) = do
		f <- safeIndex fs i1
		t <- safeIndex ts i2
		return (f, t)

readConfig :: FilePath -> IO Config
readConfig s = withConnection s $ \c -> do
	fs <- map fileFromRepr <$> query_ c "SELECT id, path FROM files"
	ts <- map tagFromRepr <$> query_ c "SELECT id, name, value FROM tags"
	m <- query_ c "SELECT fileid, tagid FROM mapping"
	let ms = findElems ts fs m
	return $ Config ts fs ms

findIds :: [Tag] -> [FilePath] -> [(FilePath, Tag)] -> [(Int, Int)]
findIds ts fs = mapMaybe helper where
	helper (f, t) = do
		i1 <- elemIndex f fs
		i2 <- elemIndex t ts
		return (i1, i2)

writeConfig :: Config -> FilePath -> IO ()
writeConfig (Config ts fs ms) s = withConnection s $ \c -> do
	createEmptyTables c

	let ms' = findIds ts fs ms
	let ts' = zip [0..] ts
	let fs' = zip [0..] fs

	forM_ fs' $ \(i, f) -> execute c
		"INSERT INTO files (id, path) VALUES (?, ?)" (fileToRepr i f)
	forM_ ts' $ \(i, t) -> execute c
		"INSERT INTO tags (id, name, value) VALUES (?, ?, ?)" (tagToRepr i t)
	forM_ ms' $ \m -> execute c
		"INSERT INTO mapping (fileid, tagid) VALUES (?, ?)" m
