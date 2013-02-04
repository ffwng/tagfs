module TagFS (
	Route, TagSet,
	Tag(..),
	getName, getValue,
	Entry(..),
	Dir(..),
	getEntryPath,
	buildBaseRoute,
	route, route',
	routeDir, routeDir',
	split, tagSep
) where

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


{- |
	A file in the tagfs file system.
	
	Currently, the following types of files occur:

	* @'RegularFile' path@ – A file, wich points to a real file with name @path@

	* @'TagFile' tags name path@ – A virtual file containing the @tags@ of the file
	  @name@. The 'TagFile' itself has the file name @path@. Usually, this will be
	  @name ++ \".tags\"@
-}
data Entry = RegularFile FilePath
	| TagFile [Tag] FilePath FilePath
	deriving (Eq, Show, Read)


{- |
	A directory in the tagfs file system.

	Currently, the following types of directories occur:

	* @'TagDir' tag@ – A directory representing the 'Tag' @tag@. Usually, this
	  will include all files with this tag, probably further filtered.

	* @'ExtendedBaseDir' name@ – An 'ExtendedTag' is representet as a path
	  @name/value@ with two directories. The @value@ directory will be the
	  'TagDir' of the tag, the @name@ directory is an @'ExtendedBaseDir' name@.

	* 'Dir' – A default directory without special meaning (such as the root
	  directory).
-}
data Dir = TagDir Tag
	| ExtendedBaseDir String
	| Dir
	deriving (Eq, Ord, Show, Read)

-- | The 'Route.Route' used for the tagfs file system. It uses 'FilePath' for the path
-- segments and @('Maybe' 'Dir')@ as directory tags.
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
data Tag = Simple String | Extended String String
	deriving (Eq, Ord, Show, Read)

-- | Extracts the name of a 'Tag'.
getName :: Tag -> String
getName (Simple n) = n
getName (Extended n _) = n

-- | Extracts the value of a 'Tag'. For simple tags, the name is returned instead.
getValue :: Tag -> String
getValue (Simple v) = v
getValue (Extended _ v) = v

-- | The 'TagSet.TagSet' used for the tagfs file system. It uses 'FilePath' for
-- file names and 'Tag' for tags.
type TagSet = T.TagSet FilePath Tag

-- | Gets the path name of an 'Entry'.
getEntryPath :: Entry -> FilePath
getEntryPath (RegularFile p) = p
getEntryPath (TagFile _ _ p) = p

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

-- | Creates the complete tagfs file system route for a given 'TagSet'.
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
	fs <- queryFilesSet f <$> gets tagSet
	s <- lift $ matchSet Nothing fs
	return $ RegularFile s


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

-- | This is an alternative file path seperator, which can be used for extended tags.
-- The current value ist \':\'. So @name/value@ and @name:value@ refer to the same path.
tagSep :: Char
tagSep = ':'

-- todo: handle ':'-handling in tagDirRoute
-- | Splits a 'FilePath' in a list of directories. The path is expected to begin
-- with \'\/\'. It is splitted on every \'\/\' (but the first one) and 'tagSep'.
split :: FilePath -> [FilePath]
split p = split' (map f p) where
	f x | x == tagSep = pathSeparator
	f x = x

split' :: FilePath -> [FilePath]
split' [] = error "split': empty list"
split' (_:ps) = splitDirectories ps

-- | Routes a given path. Performs splitting with 'split' and wraps
-- 'Route.route', giving a nicer return type.
route :: Route Entry -> FilePath -> Maybe (Either Dir Entry)
route r p = let seg = split p in route' r seg

helper :: Either (Maybe Dir) (a, t) -> Either Dir a
helper (Left e) = Left $ fromMaybe Dir e
helper (Right (a,_)) = Right a

-- | A variant of 'route', which expects the path to be splitted with 'split' already.
route' :: Route Entry -> [FilePath] -> Maybe (Either Dir Entry)
route' r seg = helper <$> R.route Nothing r seg

-- | Routes a given path wir 'Route.getBranch'. Performs splitting with 'split'
-- and wraps the result into a nicer type.
routeDir :: Route Entry -> FilePath -> Maybe (Maybe [(FilePath, Either Dir Entry)])
routeDir r p = let seg = split p in routeDir' r seg

-- | A variant of 'routeDir', wich expects the path to be splitted with 'split' already.
routeDir' :: Route Entry -> [FilePath]
	-> Maybe (Maybe [(FilePath, Either Dir Entry)])
routeDir' r seg = case getBranch Nothing r seg of
	Nothing -> Nothing
	Just (Right _) -> Just Nothing
	Just (Left es) -> Just . Just $ map (second helper) es
