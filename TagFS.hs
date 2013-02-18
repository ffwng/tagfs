module TagFS (
	Route, TagSet,
	Tag(..),
	getName, getValue,
	File(..),
	Entry(..),
	Dir(..),
	buildBaseRoute,
	route, route',
	routeDir, routeDir',
	split, tagSep
) where

import Route hiding (Route, route)
import qualified Route as R
import TagSet hiding (TagSet)
import qualified TagSet as T
import Predicate

import System.FilePath
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Set (Set)
import qualified Data.Set as S

-- | A file of the real file system.
newtype File = File { getPath :: FilePath }
	deriving (Eq, Ord, Show, Read)

{- |
	A file in the tagfs file system.
	
	Currently, the following types of files occur:

	* @'RegularFile' file@ – A file, wich points to a real file @file@

	* @'TagFile' tags file@ – A virtual file containing the @tags@ of the file @file@.
-}
data Entry = RegularFile File
	| TagFile [Tag] File
	deriving (Eq, Show, Read)


{- |
	A directory in the tagfs file system.

	Currently, the following types of directories occur:

	* @'TagDir' tag@ – A directory representing the 'Tag' @tag@. Usually, this
	  will include all files with this tag, probably further filtered.

	* @'ExtendedBaseDir' name@ – An 'ExtendedTag' is representet as a path
	  @name/value@ with two directories. The @value@ directory will be the
	  'TagDir' of the tag, the @name@ directory is an @'ExtendedBaseDir' name@.

	* 'Dir' – A default directory with no special meaning (such as the root
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
type TagSet = T.TagSet File Tag

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

matchHidden :: Maybe Dir -> FilePath -> Route ()
matchHidden t s = captureBool t (== s) >> return ()

modifyVisited :: ([Tag] -> [Tag]) -> RouteBuilder ()
modifyVisited f = modify (\s -> s { visited = f (visited s) })

modifyPredicate :: ((Set Tag -> Bool) -> Set Tag -> Bool) -> RouteBuilder ()
modifyPredicate f = modify (\s -> s { predicate = f (predicate s) })

-- | Creates the complete tagfs file system route for a given 'TagSet'.
buildBaseRoute :: TagSet -> Route Entry
buildBaseRoute ts = evalStateT buildSubRoute (makeStatus ts)

buildSubRoute :: RouteBuilder Entry
buildSubRoute = choice_ [filesRoute, tagDirsRoute, expressionRoutes]

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

expressionRoutes :: RouteBuilder Entry
expressionRoutes = choice_
	[ expressionRoute (&&)
	, lift (match Nothing "not") >> expressionRoute (&&!)
	, lift (match Nothing "and") >> expressionRoute (&&)
	, lift (match Nothing "or") >> expressionRoute (||)
	, lift (match Nothing "and" >> match Nothing "not") >> expressionRoute (&&!)
	, lift (match Nothing "or" >> match Nothing "not") >> expressionRoute (||!)
	]
	where
		a &&! b = a && not b
		a ||! b = a || not b

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
				[x] -> Just x
				_ -> Nothing

expressionRoute :: (Bool -> Bool -> Bool) -> RouteBuilder Entry
expressionRoute op = do
	tree <- lift $ capture Nothing (\s -> case s of '?':s' -> parse s'; _ -> Nothing)
	let p s c = case c of
		Exists v -> Simple v `S.member` s
		Is Equals a (StringVal b) -> Extended a b `S.member` s
		Is o a b -> findTag s o a b
	let m f s = f s `op` eval' (p s) tree
	modifyPredicate m
	buildSubRoute
	where
		findTag :: Set Tag -> Op -> Var -> Val -> Bool
		findTag s o a (StringVal b) = find (getOp o) (vals Just s a) b
		findTag s o a (IntVal b) = find (getOp o) (vals readMay s a) b

		vals :: (String -> Maybe a) -> Set Tag -> String -> [a]
		vals f s n = mapMaybe p $ S.elems s where
			p (Extended n' v) | n' == n = f v
			p _ = Nothing

		getOp :: Ord a => Op -> a -> a -> Bool
		getOp Equals = (==)
		getOp Greater = (>)
		getOp Less = (<)
		getOp GreaterThan = (>=)
		getOp LessThan = (<=)

		find :: Ord a => (a -> a -> Bool) -> [a] -> a -> Bool
		find o l b = any (`o` b) l



tagDir :: Tag -> RouteBuilder ()
tagDir tag@(Simple n) = lift $ match (Just $ TagDir tag) n
tagDir tag@(Extended n v) =
	lift (matchHidden (Just $ TagDir tag) (n ++ tagSep:v)) <|>
	lift (do
		match (Just $ ExtendedBaseDir n) n
		match (Just $ TagDir tag) v)

regularFileRoute :: RouteBuilder Entry
regularFileRoute = do
	f <- gets predicate
	fs <- queryFilesSet f <$> gets tagSet
	s <- lift $ matchSet Nothing (S.map getPath fs)
	return $ RegularFile (File s)


tagFileExt :: FilePath
tagFileExt = ".tags"

tagFileRoute :: RouteBuilder Entry
tagFileRoute = do
	file <- lift $ capture Nothing getFile
	f <- gets predicate
	ts <- gets tagSet
	let fs = queryFiles f ts
	if file `notElem` fs then lift noRoute
	else do
		let t = queryTags file ts
		lift $ maybe noRoute (\t' -> return $ TagFile t' file) t
		where
			getFile n = case splitExtension n of
				(name, ext) | ext == tagFileExt -> Just $ File name
				_ -> Nothing

-- helper function for easier routing

-- | Represents the seperator used to seperate name and value of extended tags.
-- The current value is ':'.
tagSep :: Char
tagSep = ':'

-- | Splits a 'FilePath' in a list of directories. The path is expected to begin
-- with \'\/\'. It is splitted on every \'\/\' (but the first one).
split :: FilePath -> [FilePath]
split [] = error "split': empty list"
split (_:ps) = splitDirectories ps

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
