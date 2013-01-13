module TagFS where

import Route
import TagSet

import System.IO
import System.FilePath
import Data.List

data Entry = RegularFile FilePath
	| TagFile [Tag] FilePath
	| TagDir [Tag] [Entry] FilePath
	| OtherDir [Entry] FilePath
	| DirName FilePath
	deriving Show

getPath :: Entry -> FilePath
getPath (RegularFile p) = p
getPath (TagFile _ p) = p
getPath (TagDir _ _ p) = p
getPath (OtherDir _ p) = p
getPath (DirName p) = p

getDirEntries :: Entry -> Maybe [Entry]
getDirEntries (TagDir _ e _) = Just e
getDirEntries (OtherDir e _) = Just e
getDirEntries _ = Nothing

toRoute :: (FilePath -> Entry) -> FilePath -> Route Entry
toRoute f name = match name >> return (f name)

regularFile :: FilePath -> Route Entry
regularFile = toRoute RegularFile

tagDirEntries :: [Tag] -> TagSet -> [Entry]
tagDirEntries t ts = tagdirs ++ regfiles where
		tagdirs = map DirName (tags ts \\ t)
		regfiles = map RegularFile (files ts)

buildBaseRoute :: TagSet -> Route Entry
buildBaseRoute ts = choice [basedir, buildSubRoute [] ts] where
	basedir = eor >> return (OtherDir (tagDirEntries [] ts) "/")

buildSubRoute :: [Tag] -> TagSet -> Route Entry
buildSubRoute visited ts = choice [fileRoute ts, tagDirRoute visited ts, tagFileRoute ts]

tagDirRoute :: [Tag] -> TagSet -> Route Entry
tagDirRoute visited ts = choice . map get $ filter (`notElem` visited) (tags ts) where
	get tag = match tag >> choice [dir, content] where
		queried = query tag ts
		visited' = tag:visited
		dir = eor >> return (TagDir visited' (tagDirEntries visited' queried) tag)
		content = buildSubRoute (tag:visited) queried

fileRoute :: TagSet -> Route Entry
fileRoute ts = choice $ map get (files ts) where
	get file = regularFile file

tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: TagSet -> Route Entry
tagFileRoute ts = do
	name <- capture $ getName . splitExtension
	let t = queryTags name ts
	case t of
		Just t' -> return $ TagFile t' name
		Nothing -> noRoute
	where
		getName (name, ext) | ext == tagFileExt = Just name
		getName _ = Nothing


-- helper function for easier routing

--data Dir = Dir [FilePath] [Entry]

route :: Route Entry -> FilePath -> Maybe Entry
route r (p:ps) = let seg = splitDirectories ps in runRoute r seg

{-main = do
	let ts = fromFiles ["a"] [("1", [])]
	let r = buildBaseRoute ts
	let res = runRoute r ["1.tags", "1.tags"]
	case res of
		Nothing -> print "nothing"
		Just _ -> print "just"
-}

