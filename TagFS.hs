module TagFS where

import Route
import TagSet

import System.IO
import System.FilePath

data FileEntry = RegularFile FilePath | TagFile [Tag] FilePath

toRoute :: (FilePath -> FileEntry) -> FilePath -> Route FileEntry
toRoute f name = match name >> return (f name)

regularFile :: FilePath -> Route FileEntry
regularFile = toRoute RegularFile

tagFile :: [Tag] -> FilePath -> Route FileEntry
tagFile t = toRoute $ TagFile t

buildBaseRoute :: TagSet -> Route FileEntry
buildBaseRoute = buildRoute

buildRoute :: TagSet -> Route FileEntry
buildRoute ts = choice [tagDirRoute ts, fileRoute ts, tagFileRoute ts]

tagDirRoute :: TagSet -> Route FileEntry
tagDirRoute ts = choice $ map get (tags ts) where
	get tag = match tag >> buildRoute (query tag ts)

fileRoute :: TagSet -> Route FileEntry
fileRoute ts = choice $ map get (files ts) where
	get file = regularFile file

tagFileRoute :: TagSet -> Route FileEntry
tagFileRoute ts = do
	name <- capture $ getName . splitExtension
	let ts = queryTags name ts
	case ts of
		Just t -> tagFile t name
		Nothing -> noRoute
	where
		getName (name, ".tags") = Just name
		getName _ = Nothing

