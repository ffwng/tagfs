module TagFS where

import Route
import TagSet

import System.IO
import System.FilePath
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Function (on)
import Control.Monad.Trans
import Control.Monad.Trans.State

data Entry = RegularFile FilePath
	| TagFile [Tag] FilePath FilePath
	| DirName FilePath
	deriving (Show, Eq)

data Dir = TagDir Tag
	| ExtendedBaseDir String
	| Dir
	deriving (Show, Eq)

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


data FSStatus = FSStatus { tagSet :: TagSet, visited :: [Tag] }

makeStatus :: TagSet -> FSStatus
makeStatus ts = FSStatus ts []

type RouteBuilder = StateT FSStatus (Route Dir)

choice_ :: [RouteBuilder a] -> RouteBuilder a
--choice_ list = lift (choice <$> sequence list)
choice_ l = do
	state <- get
	(a, state') <- lift . choice $ map (`runStateT` state) l
	put state'
	return a

modifyVisited :: ([Tag] -> [Tag]) -> RouteBuilder ()
modifyVisited f = modify (\s -> s { visited = f (visited s) })

modifyTagSet :: (TagSet -> TagSet) -> RouteBuilder ()
modifyTagSet f = modify (\s -> s { tagSet = f (tagSet s) })

buildBaseRoute :: TagSet -> Route Dir Entry
buildBaseRoute ts = foldRoute $ evalStateT buildSubRoute (makeStatus ts)

buildSubRoute :: RouteBuilder Entry
buildSubRoute = choice_ [filesRoute, tagDirsRoute]

filesRoute :: RouteBuilder Entry
filesRoute = choice_ [regularFileRoute, tagFileRoute]

tagDirsRoute :: RouteBuilder Entry
tagDirsRoute = do
	ts <- gets tagSet
	visit <- gets visited
	let mytags = filter (`notElem` visit) (tags ts)
	choice_ (map tagRoute mytags)

tagRoute t = choice_ [plainTagRoute t, logicalDirsRoute t]
	
plainTagRoute :: Tag -> RouteBuilder Entry
plainTagRoute tag = do
	tagDir tag
	-- todo: could be done better?
	--modify (\s -> s { visited = tag:(visited s), tagSet = query tag (tagSet s) })
	modifyVisited (tag:)
	modifyTagSet (query tag)
	buildSubRoute

logicalDirsRoute :: Tag -> RouteBuilder Entry
logicalDirsRoute tag = do
	ts <- gets tagSet
	visit <- gets visited
	let mytags = filter (`notElem` (tag:visit)) (tags ts)
	tagDir tag
	choice_
		[ lift (match "and") >> choice_ (map
			(logicalTagRoute (\t tags' -> t `elem` tags' && tag `elem` tags')) mytags)
		, lift (match "or") >> choice_ (map
			(logicalTagRoute (\t tags' -> t `elem` tags' || tag `elem` tags')) mytags)
		, lift (match "not") >> choice_ (map
			(logicalTagRoute (\t tags' -> t `notElem` tags' && tag `elem` tags')) mytags)
		]

logicalTagRoute :: (Tag -> [Tag] -> Bool) -> Tag -> RouteBuilder Entry
logicalTagRoute f tag = do
	tagDir tag
	--modify (\s -> s { visited = tag:(visited s), tagSet = queryBy (f tag) (tagSet s) })
	modifyVisited (tag:)
	modifyTagSet (queryBy (f tag))
	buildSubRoute

tagDir :: Tag -> RouteBuilder ()
tagDir tag@(Simple n) = lift $ do
	match n
	dir $ TagDir tag
tagDir tag@(Extended n v) = lift $ do
	match n
	dir $ ExtendedBaseDir n
	match v
	dir $ TagDir tag

regularFileRoute :: RouteBuilder Entry
regularFileRoute = do
	ts <- gets tagSet
	lift $ choice $ map get (files ts) where
		get file = regularFile file

tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: RouteBuilder Entry
tagFileRoute = do
	ts <- gets tagSet
	(name, path) <- lift $ capture getName
	let t = queryTags name ts
	lift $ maybe noRoute (\t' -> return $ TagFile t' name path) t
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

