module TagFS where

import Route
import TagSet

import System.IO
import System.FilePath

data FileEntry = RegularFile FilePath | TagFile [Tag] FilePath

getPath :: FileEntry -> FilePath
getPath (RegularFile p) = p
getPath (TagFile _ p) = p

toRoute :: (FilePath -> FileEntry) -> FilePath -> Route FileEntry
toRoute f name = match name >> return (f name)

regularFile :: FilePath -> Route FileEntry
regularFile = toRoute RegularFile

tagFile :: [Tag] -> FilePath -> Route FileEntry
tagFile t = toRoute $ TagFile t

buildBaseRoute :: TagSet -> Route FileEntry
buildBaseRoute = buildSubRoute []

buildSubRoute :: [Tag] -> TagSet -> Route FileEntry
buildSubRoute visited ts = choice [fileRoute ts, tagDirRoute visited ts, tagFileRoute ts]

tagDirRoute :: [Tag] -> TagSet -> Route FileEntry
tagDirRoute visited ts = choice . map get $ filter (`notElem` visited) (tags ts) where
	get tag = match tag >> buildSubRoute (tag:visited) (query tag ts)

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


-- helper function for easier routing

data Dir = Dir [FilePath] [FileEntry]

route :: Route FileEntry -> FilePath -> Maybe (Either Dir FileEntry)
route r (p:ps) = let seg = splitDirectories ps in
	case runRoute r seg of
		Nothing -> Nothing
		Just (Right x) -> Just (Right x)
		Just (Left dir) -> case getSegments dir of
			Nothing -> Nothing   -- should never happen
			Just paths -> Just . Left $ mkDir paths
	where
		mkDir :: [(String, Route FileEntry)] -> Dir
		mkDir = go (Dir [] [])
		go d [] = d
		go (Dir d f) ((s,r):xs) = case getLeaf r of
			Nothing -> go (Dir (s:d) f) xs
			Just e -> go (Dir d (e:f)) xs

{-main = do
	let ts = fromFiles ["a"] [("1", [])]
	let r = buildBaseRoute ts
	let res = runRoute r ["1.tags", "1.tags"]
	case res of
		Nothing -> print "nothing"
		Just _ -> print "just"
-}

