module TagFS where

import Route
import TagSet

import System.IO
import System.FilePath
import Data.List
import Control.Applicative
import Data.Maybe

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
		choice (map tagroute . filter (`notElem` visited) $ (tags ts))
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
	maybe noRoute (\t' -> return $ TagFile t' name path) t
	where
		getName n = case splitExtension n of
			(name, ext) | ext == tagFileExt -> Just (name, n)
			_ -> Nothing


-- helper function for easier routing

route :: Route Entry -> FilePath -> Maybe Entry
route r (p:ps) = let seg = splitDirectories ps in route' r seg

route' :: Route Entry -> [FilePath] -> Maybe Entry
route' r seg = fromMaybe Dir <$> runRoute r seg

routeDir :: Route Entry -> FilePath -> Maybe (Maybe [Entry])
routeDir r (p:ps) = let seg = splitDirectories ps in routeDir' r seg

routeDir' :: Route Entry -> [FilePath] -> Maybe (Maybe [Entry])
routeDir' r seg = fmap (fmap (map entry)) $ getRestSegments r seg where
	entry (_, Just a) = a
	entry (s, Nothing) = DirName s

