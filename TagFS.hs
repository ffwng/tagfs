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
	| DirName FilePath
	deriving Show

data Dir = TagDir Tag
	| ExtendedBaseDir String
	| Dir

getPath :: Entry -> FilePath
getPath (RegularFile p) = p
getPath (TagFile _ _ p) = p
getPath (DirName p) = p

isDir :: Entry -> Bool
isDir (DirName _) = True
isDir _ = False

toRoute :: (FilePath -> Entry) -> FilePath -> Route Dir Entry
toRoute f name = match name >> return (f name)

regularFile :: FilePath -> Route Dir Entry
regularFile = toRoute RegularFile

dir :: Dir -> Route Dir ()
dir = tag

buildBaseRoute :: TagSet -> Route Dir Entry
buildBaseRoute ts = foldRoute $ buildSubRoute [] ts

buildSubRoute :: [Tag] -> TagSet -> Route Dir Entry
buildSubRoute visited ts = choice [fileRoute ts, tagDirRoute visited ts, tagFileRoute ts]

tagDirRoute :: [Tag] -> TagSet -> Route Dir Entry
tagDirRoute visited ts = tagsroute where
	mytags = filter (`notElem` visited) (tags ts)
	tagsroute = choice $ map tagroute mytags
	tagroute tag@(Simple n) = subroute tag
	tagroute tag@(Extended n v) = do
		match n
		dir $ ExtendedBaseDir n
		subroute tag
	subroute tag = do
		match $ getValue tag
		dir $ TagDir tag
		buildSubRoute (tag:visited) (query tag ts)

fileRoute :: TagSet -> Route Dir Entry
fileRoute ts = choice $ map get (files ts) where
	get file = regularFile file

tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: TagSet -> Route Dir Entry
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

route :: Route Dir Entry -> FilePath -> Maybe (Either Dir Entry)
route r p = let seg = split p in route' r seg where

route' :: Route Dir Entry -> [FilePath] -> Maybe (Either Dir Entry)
route' r seg = mapLeft (fromMaybe Dir) <$> runRoute r seg where
	mapLeft :: (a -> c) -> Either a b -> Either c b
	mapLeft f (Left x) = Left (f x)
	mapLeft _ (Right r) = Right r

routeDir :: Route Dir Entry -> FilePath -> Maybe (Maybe [Entry])
routeDir r p = let seg = split p in routeDir' r seg

routeDir' :: Route Dir Entry -> [FilePath] -> Maybe (Maybe [Entry])
routeDir' r seg = fmap (fmap (map entry)) $ getRestSegments r seg where
	entry (_, Just a) = a
	entry (s, Nothing) = DirName s

