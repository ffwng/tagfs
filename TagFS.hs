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

{-buildDir :: ([Entry] -> Entry) -> [(FilePath, Route Entry)] -> [(FilePath, Entry)]
	-> Route Entry
buildDir f dirs files = choice [makedir, makeroutes] where
	makedir = eor >> return (f $ map (DirName . fst) dirs ++ map snd files)
	makeroutes = choice $ map (\(x,r) -> match x >> r) dirs
		++ map (\(x,r) -> match x >> return r) files
-}

dir :: Entry -> Route Entry -> Route Entry
dir e r = choice [eor >> return e, r]

buildBaseRoute :: TagSet -> Route Entry
buildBaseRoute = tagDirRoute []

buildSubRoute :: [Tag] -> TagSet -> Route Entry
buildSubRoute visited ts = choice [fileRoute ts, tagDirRoute visited ts, tagFileRoute ts]

tagDirRoute :: [Tag] -> TagSet -> Route Entry
{-tagDirRoute visited ts = buildDir (TagDir visited)
	[("tags", tagsroute)] filesroute where
	tagsroute = buildDir Dir (map tagroute . filter
		(`notElem` visited) $ (tags ts)) []
	tagroute tag = (tag, buildSubRoute (tag:visited) (query tag ts))
	filesroute = map (\n -> (n, RegularFile n)) (files ts)
-}
tagDirRoute visited ts = dir (TagDir visited) (choice [tagsroute, filesroute]) where
	tagsroute = match "tags" >>
		dir Dir (choice (map tagroute . filter (`notElem` visited) $ (tags ts)))
	tagroute tag = match tag >> buildSubRoute (tag:visited) (query tag ts)
	filesroute = choice $ map regularFile (files ts)

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

route :: Route Entry -> FilePath -> Maybe (Either [Entry] Entry)
route r (p:ps) = let seg = splitDirectories ps in case runRoute r seg of
	Nothing -> Nothing
	Just (Right a) -> Just (Right a)
	Just (Left a) -> Just . Left $ map entry a
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

