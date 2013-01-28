module TagFS where

import Route hiding (Route, route)
import qualified Route as R
import TagSet hiding (TagSet)
import qualified TagSet as T

import System.FilePath
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Set (Set)
import qualified Data.Set as S

data Entry = RegularFile FilePath
	| TagFile [Tag] FilePath FilePath
	deriving (Show, Eq)

data Dir = TagDir Tag
	| ExtendedBaseDir String
	| Dir
	deriving (Show, Ord, Eq)

type Route = R.Route FilePath (Maybe Dir)

{- |
	A tag of a file. There are two possible forms of tags:

	* Simple tags: these tags have a name and act as simple
	  on/off tags. Either they are present or not for a given file. A file may not
	  have two simple tags of the same name.

	* Extended tags: these tags have a name and an associated 'String' value.
	  This way, further information can be provided, when these tags are present.
	  A file can have multiple extended tags with the same name but different values,
	  but no two extended tags of the same name and value.

	It is possible but highly discouraged to have a file with a simple and an
	extended tag of the same name.
-}
data Tag = Simple String | Extended String String deriving (Eq, Ord, Show)

-- | Extracts the name of a 'Tag'.
getName :: Tag -> String
getName (Simple n) = n
getName (Extended n _) = n

-- | Extracts the value of a 'Tag'. For simple tags, the name is returned instead.
getValue :: Tag -> String
getValue (Simple v) = v
getValue (Extended _ v) = v

type TagSet = T.TagSet FilePath Tag

getEntryPath :: Entry -> FilePath
getEntryPath (RegularFile p) = p
getEntryPath (TagFile _ _ p) = p

toRoute :: (FilePath -> Entry) -> FilePath -> Route Entry
toRoute f name = match Nothing name >> return (f name)

regularFile :: FilePath -> Route Entry
regularFile = toRoute RegularFile

allTags :: Set Tag -> Bool
allTags = const True

data FSStatus = FSStatus { tagSet :: TagSet, visited :: [Tag], predicate :: Set Tag -> Bool }

makeStatus :: TagSet -> FSStatus
makeStatus ts = FSStatus ts [] allTags

type RouteBuilder a = StateT FSStatus Route a

choice_ :: [RouteBuilder a] -> RouteBuilder a
choice_ l = do
	state1 <- get
	(a, state2) <- lift . choice $ map (`runStateT` state1) l
	put state2
	return a

modifyVisited :: ([Tag] -> [Tag]) -> RouteBuilder ()
modifyVisited f = modify (\s -> s { visited = f (visited s) })

modifyPredicate :: ((Set Tag -> Bool) -> Set Tag -> Bool) -> RouteBuilder ()
modifyPredicate f = modify (\s -> s { predicate = f (predicate s) })

buildBaseRoute :: TagSet -> Route Entry
buildBaseRoute ts = evalStateT buildSubRoute (makeStatus ts)

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

tagRoute :: Tag -> RouteBuilder Entry
tagRoute t = choice_ [plainTagRoute t, logicalDirsRoute t]
	
plainTagRoute :: Tag -> RouteBuilder Entry
plainTagRoute tag = do
	tagDir tag
	modifyVisited (tag:)
	modifyPredicate (\f s -> f s && S.member tag s)
	buildSubRoute

logicalDirsRoute :: Tag -> RouteBuilder Entry
logicalDirsRoute tag = choice_
	[ logicalTagRoute (\f s -> f s && S.member tag s) "and" tag
	, logicalTagRoute (\f s -> f s || S.member tag s) "or" tag
	, logicalTagRoute (\f s -> f s && not (S.member tag s)) "not" tag
	, lift (match Nothing "and")
		>> logicalTagRoute (\f s -> f s && not (S.member tag s)) "not" tag
	, lift (match Nothing "or")
		>> logicalTagRoute (\f s -> f s || not (S.member tag s)) "not" tag
	]


logicalTagRoute :: ((Set Tag -> Bool) -> Set Tag -> Bool) -> String -> Tag
	-> RouteBuilder Entry
logicalTagRoute f funcname tag = do
	lift (match Nothing funcname)
	tagDir tag
	modifyVisited (tag:)
	modifyPredicate f
	buildSubRoute

tagDir :: Tag -> RouteBuilder ()
tagDir tag@(Simple n) = lift $ match (Just $ TagDir tag) n
tagDir tag@(Extended n v) = lift $ do
	match (Just $ ExtendedBaseDir n) n
	match (Just $ TagDir tag) v

regularFileRoute :: RouteBuilder Entry
regularFileRoute = do
	f <- gets predicate
	fs <- queryFiles f <$> gets tagSet
	lift $ choice $ map regularFile fs

tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: RouteBuilder Entry
tagFileRoute = do
	(name, path) <- lift $ capture Nothing splitName
	f <- gets predicate
	ts <- gets tagSet
	let fs = queryFiles f ts
	if name `notElem` fs then lift noRoute
	else do
		let t = queryTags name ts
		lift $ maybe noRoute (\t' -> return $ TagFile t' name path) t
		where
			splitName n = case splitExtension n of
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
split' [] = error "split': empty list"
split' (_:ps) = splitDirectories ps

route :: Route Entry -> FilePath -> Maybe (Either Dir Entry)
route r p = let seg = split p in route' r seg

helper :: Either (Maybe Dir) (a, t) -> Either Dir a
helper (Left e) = Left $ fromMaybe Dir e
helper (Right (a,_)) = Right a

route' :: Route Entry -> [FilePath] -> Maybe (Either Dir Entry)
route' r seg = helper <$> R.route Nothing r seg

routeDir :: Route Entry -> FilePath -> Maybe (Maybe [(FilePath, Either Dir Entry)])
routeDir r p = let seg = split p in routeDir' r seg

routeDir' :: Route Entry -> [FilePath]
	-> Maybe (Maybe [(FilePath, Either Dir Entry)])
routeDir' r seg = case getBranch Nothing r seg of
	Nothing -> Nothing
	Just (Right _) -> Just Nothing
	Just (Left es) -> Just . Just $ map (second helper) es

{-route :: Route a -> FilePath -> Maybe (Either Dir Entry)
route r p = let seg = split p in route' r seg where

route' :: Route a -> [FilePath] -> Maybe (Either Dir Entry)
route' r seg = case runRoute r seg of
	(Just e, _, _) -> Just $ Right e
	(_, Just t, _) -> Just $ Left t
	_ -> Nothing

routeDir :: Route a -> FilePath -> Maybe (Maybe [Leaf])
routeDir r p = let seg = split p in routeDir' r seg

routeDir' :: Route a -> [FilePath] -> Maybe (Maybe [Leaf])
routeDir' r seg = case runRoute r seg of
	(Just _, _, _) -> Just Nothing
	(_, _, Just e) -> Just . Just $ map leaf e
	_ -> Nothing
	where
		leaf (p, Nothing) = File p
		leaf (p, _) = Dir p
-}
