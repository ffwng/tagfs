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
import Data.Set (Set)
import qualified Data.Set as S

data Entry = RegularFile FilePath
	| TagFile [Tag] FilePath FilePath
	| DirName FilePath
	deriving (Show, Eq)

data Dir = TagDir Tag
	| ExtendedBaseDir String
	| Dir
	deriving (Show, Ord, Eq)

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

allTags :: Set Tag -> Bool
allTags = const True

data FSStatus = FSStatus { tagSet :: TagSet, visited :: [Tag], predicate :: Set Tag -> Bool }

makeStatus :: TagSet -> FSStatus
makeStatus ts = FSStatus ts [] allTags

type RouteBuilder = StateT FSStatus (Route Dir)

choice_ :: [RouteBuilder a] -> RouteBuilder a
choice_ l = do
	state <- get
	(a, state') <- lift . choice $ map (`runStateT` state) l
	put state'
	return a

modifyVisited :: ([Tag] -> [Tag]) -> RouteBuilder ()
modifyVisited f = modify (\s -> s { visited = f (visited s) })

modifyPredicate :: ((Set Tag -> Bool) -> (Set Tag -> Bool)) -> RouteBuilder ()
modifyPredicate f = modify (\s -> s { predicate = f (predicate s) })

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
	modifyVisited (tag:)
	modifyPredicate (\f s -> f s && S.member tag s)
	buildSubRoute

logicalDirsRoute :: Tag -> RouteBuilder Entry
logicalDirsRoute tag = do
	choice_
		[ logicalTagRoute (\f s -> f s && S.member tag s) "and" tag
		, logicalTagRoute (\f s -> f s || S.member tag s) "or" tag
		, logicalTagRoute (\f s -> f s && not (S.member tag s)) "not" tag
		, lift (match "and")
			>> logicalTagRoute (\f s -> f s && not (S.member tag s)) "not" tag
		, lift (match "or")
			>> logicalTagRoute (\f s -> f s || not (S.member tag s)) "not" tag
		]


logicalTagRoute :: ((Set Tag -> Bool) -> (Set Tag -> Bool)) -> String -> Tag
	-> RouteBuilder Entry
logicalTagRoute f funcname tag = do
	lift (match funcname)
	tagDir tag
	modifyVisited (tag:)
	modifyPredicate f
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
	f <- gets predicate
	files <- queryFiles f <$> gets tagSet
	lift $ choice $ map get files where
		get file = regularFile file

tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: RouteBuilder Entry
tagFileRoute = do
	(name, path) <- lift $ capture getName
	f <- gets predicate
	ts <- gets tagSet
	let files = queryFiles f ts
	if name `notElem` files then lift noRoute
	else do
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

route :: Route Dir Entry -> FilePath -> IO (Maybe (Either Dir Entry))
route r p = let seg = split p in route' r seg where

route' :: Route Dir Entry -> [FilePath] -> IO (Maybe (Either Dir Entry))
route' r seg = (fmap $ mapLeft (fromMaybe Dir)) <$> runRoute r seg where
	mapLeft :: (a -> c) -> Either a b -> Either c b
	mapLeft f (Left x) = Left (f x)
	mapLeft _ (Right r) = Right r

routeDir :: Route Dir Entry -> FilePath -> IO (Maybe (Maybe [Entry]))
routeDir r p = let seg = split p in routeDir' r seg

routeDir' :: Route Dir Entry -> [FilePath] -> IO (Maybe (Maybe [Entry]))
routeDir' r seg = fmap (>>= go) <$> runTag r seg where
	go (t, r) = let x = getRestSegments r in
		-- anything with a tag is a directory
		if isJust t then Just . map entry $ fromMaybe [] x
		else map entry <$> x
	entry (_, Just a) = a
	entry (s, Nothing) = DirName s

