module TagFS where

import Route
import TagSet

import System.IO
import System.FilePath
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Function (on)

data Entry = RegularFile FilePath
	| TagFile [Tag] FilePath FilePath
	| TagDir Tag
	| ExtendedBaseDir String
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
isDir (ExtendedBaseDir _) = True
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
buildBaseRoute ts = foldRoute $ buildSubRoute [] ts

buildSubRoute :: [Tag] -> TagSet -> Route Entry
buildSubRoute visited ts = choice [fileRoute ts, tagDirRoute visited ts, tagFileRoute ts]

tagDirRoute :: [Tag] -> TagSet -> Route Entry
tagDirRoute visited ts = tagsroute where
	mytags = filter (`notElem` visited) (tags ts)
	{-tagsroute = let (s, e) = splitTags mytags in
		choice $ map simpleroute s ++ map extendedroute e
	simpleroute tag = do
		match (getValue tag)
		dir (TagDir tag) $ subroute tag
	extendedroute (name, tags) = do
		match name
		dir (ExtendedBaseDir name) $ do
			choice $ map (simpleroute) tags
	subroute tag = buildSubRoute (tag:visited) (query tag ts)-}
	tagsroute = choice $ map tagroute mytags
	tagroute tag@(Simple n) = subroute tag
	tagroute tag@(Extended n v) = do
		match n
		dir (ExtendedBaseDir n) $ subroute tag
	subroute tag = do
		match $ getValue tag
		dir (TagDir tag) $ buildSubRoute (tag:visited) (query tag ts)

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

tagSep :: Char
tagSep = ':'

-- todo: handle ':'-handling in tagDirRoute
split :: FilePath -> [String]
split p = split' (map f p) where
	f x | x == tagSep = pathSeparator
	f x = x

split' :: FilePath -> [String]
split' (p:ps) = splitDirectories ps

route :: Route Entry -> FilePath -> Maybe Entry
route r p = let seg = split p in route' r seg where

route' :: Route Entry -> [FilePath] -> Maybe Entry
route' r seg = fromMaybe Dir <$> runRoute r seg

routeDir :: Route Entry -> FilePath -> Maybe (Maybe [Entry])
routeDir r p = let seg = split p in routeDir' r seg

routeDir' :: Route Entry -> [FilePath] -> Maybe (Maybe [Entry])
routeDir' r seg = fmap (fmap (map entry)) $ getRestSegments r seg where
	entry (_, Just a) = a
	entry (s, Nothing) = DirName s

