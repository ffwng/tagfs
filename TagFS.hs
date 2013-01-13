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

tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: TagSet -> Route FileEntry
tagFileRoute ts = do
	name <- capture $ getName . splitExtension
	let t = queryTags name ts
	case t of
		Just t' -> return $ TagFile t' name
		Nothing -> noRoute
	where
		getName (name, ext) | ext == tagFileExt = Just name
		getName _ = Nothing

{-main = do
	let ts = fromFiles ["a"] [("1", [])]
	let r = buildBaseRoute ts
	let res = runRoute r ["1.tags", "1.tags"]
	case res of
		Nothing -> print "nothing"
		Just _ -> print "just"
-}
