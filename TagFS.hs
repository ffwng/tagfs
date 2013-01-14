module TagFS where

import Route
import TagSet

import System.IO
import System.FilePath
import Data.List

data Entry = RegularFile FilePath
	| TagFile [Tag] FilePath FilePath
	| TagDir [Tag]
	| Dir
	| DirName FilePath
	deriving Show

getPath :: Entry -> Maybe FilePath
getPath (RegularFile p) = Just p
getPath (TagFile _ _ p) = Just p
getPath (DirName p) = Just p
getPath _ = Nothing

isDir :: Entry -> Bool
isDir (TagDir _) = True
isDir Dir = True
isDir (DirName _) = True
isDir _ = False

toRoute :: (FilePath -> Entry) -> FilePath -> Route Entry
toRoute f name = match name >> return (f name)

regularFile :: FilePath -> Route Entry
regularFile = toRoute RegularFile

dir :: Entry -> Route Entry -> Route Entry
dir e r = choice [eor >> return e, r]

buildBaseRoute :: TagSet -> Route Entry
buildBaseRoute = buildSubRoute []

buildSubRoute :: [Tag] -> TagSet -> Route Entry
buildSubRoute visited ts = choice [fileRoute ts, tagDirRoute visited ts, tagFileRoute ts]

tagDirRoute :: [Tag] -> TagSet -> Route Entry
tagDirRoute visited ts = dir (TagDir visited) tagsroute where
	tagsroute = --match "tags" >>
		dir Dir (choice (map tagroute . filter (`notElem` visited) $ (tags ts)))
	tagroute tag = match tag >> buildSubRoute (tag:visited) (query tag ts)

fileRoute :: TagSet -> Route Entry
fileRoute ts = choice $ map get (files ts) where
	get file = regularFile file

tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: TagSet -> Route Entry
tagFileRoute ts = do
	(name, path) <- capture getName
	let t = queryTags name ts
	case t of
		Just t' -> return $ TagFile t' name path
		Nothing -> noRoute
	where
		getName n = case splitExtension n of
			(name, ext) | ext == tagFileExt -> Just (name, n)
			_ -> Nothing


-- helper function for easier routing

--data Dir = Dir [FilePath] [Entry]

route :: Route Entry -> FilePath -> Maybe Entry
route r (p:ps) = let seg = splitDirectories ps in case runRoute r seg of
	Nothing -> Nothing
	a -> a

routeDir :: Route Entry -> FilePath -> Maybe [Entry]
routeDir r (p:ps) = let seg = splitDirectories ps in case getRestSegments r seg of
	Nothing -> Nothing
	Just entries -> Just $ map entry entries
	where
		entry (_, Just a) = a
		entry (s, Nothing) = DirName s

{-main = do
	let ts = fromFiles ["a"] [("1", [])]
	let r = buildBaseRoute ts
	let res = runRoute r ["1.tags", "1.tags"]
	case res of
		Nothing -> print "nothing"
		Just _ -> print "just"
-}

