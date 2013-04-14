{-# LANGUAGE DeriveFunctor, TupleSections, GeneralizedNewtypeDeriving, Trustworthy #-}
module Route (
	Route,
	route,
	getBranch,
	match, matchHidden, matchSet, capture,
	captureBool, choice, noRoute
) where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Function
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S

data Segment s t a =
	  Capture [s] t (s -> Maybe a)
	| Choice [a]
	| NoRoute
	deriving (Functor)

{- |
	Represents a route.
	
	A @'Route' s t a@ provides a mapping from paths to values of type a. A /path/
	is a list of segments of type @s@, whereby the last segment represents the value itself
	and previous segments represent intermediate steps to get to it. The concept is
	comparable to file system paths, where the segments are a path component, the last
	segment is a file name, the privious segments are directories and the value is the
	file itself.
	
	Each path segment is annotated with a tag of type @t@. This makes it possible to
	determine and distinguish intermediate steps and handle routes which are not finished.

	Therefore, one can think of a route as
	@type Route s t a = [([(s, t)], a)]@, i.e. a list of paths consisting of segments
	annotated with a tag and a value associated to this path.
	The implementation uses a tree data structure, which has identical behavior, but
	offers better time and space metrics.

	If one does not wish to tag every element, @'Maybe' t'@ for some @t'@ can be used.
	@()@ as @t@ would lead to no tags at all.
-}
newtype Route s t a = Route { unRoute :: Free (Segment s t) a }
	deriving (Functor, Applicative, Monad)

-- | Note: for more than two alternatives, 'choice' is more efficient than 'msum'.
instance MonadPlus (Route s t) where
	mzero = noRoute
	mplus a b = choice [a, b]

{- |
	Routes a path in a route. The following cases may occur when doing this:

	* The route contains the path. In this case @Just (Right (a,t))@ is returned,
	  where @a@ is the value of the path and @t@ the tag of the last segment.

	* The path is a proper prefix of a path in the route, i.e. the route has not
	  finished. In this case, @Just (Left t)@ is returned, where @t@ is the tag
	  of the last matched segment.

	* The path in neither contained in the route, nor is it a proper prefix of
	  some other route's path. In this case, @Nothing@ is returned.

	If a path statisfies both of the former cases, the first one applicable is used.
	In particular, if @s@ is already determined to be a proper prefix of some other
	path, it is not checked whether @s@ is contained in the route itself.

	The @t@ parameter represents the tag of the “base directory” (which cannot be
	represented in the route itself with the above definition). It is only used
	when @s == []@.

	Routes provid a @Monad@ instance to make them easily composed. For instance
	when given

@
r :: Route String Char Int 
r = do
	match \'a\' \"foo\"
	match \'b\' \"bar\"
	choice [match \'c\' \"foobar\" >> return 0,
		match \'d\' \"barfoo\" >> match \'e\' \"barfoo2\" >> return 1]
@

	@r@ would consist of the following paths:

	* [\"foo\", \"bar\", \"foobar\"] returning (0, \'c\')

	* [\"foo\", \"bar\", \"barfoo\", \"barfoo2\"] returning (1, \'d\')

-}
route :: Ord s => t -> Route s t a -> [s] -> Maybe (Either t (a, t))
route bt r bs = get bt bs (unRoute r) where
	get t [] (Pure a) = Just $ Right (a, t)
	get t [] _ = Just $ Left t
	get t xs (Free (Choice as)) = msum $ map (get t xs) as
	get _ (x:xs) (Free (Capture _ t f)) = get t xs =<< f x
	get _ _ _ = Nothing

-- nubs a sorted list
sortedNubBy :: (a -> a -> Bool) -> [a] -> [a]
sortedNubBy _ [] = []
sortedNubBy _ [x] = [x]
sortedNubBy f (a:b:xs) | f a b = sortedNubBy f (b:xs)
sortedNubBy f (a:b:xs) = a : sortedNubBy f (b:xs)

{- |
	Routes a path in a route. It behaves exactly as 'route', except for the
	proper-prefix-case. In this case, instead of the tag of the last segment a
	list [(s, Either t (a,t)] is returned, where each element represents possible
	continuation of the route. The @s@ part indicates the next segment, the second
	element of the tuple is either @Right (a, t)@ if this segment would finish
	the route with element @a@ or @Left t@ if not. Either way, the @t@ represents
	the tag of the next segment @s@.

	For example: if given @r@ like above, @getBranch \'x\' r [\"foo\", \"bar\"]@
	would be @Just (Left [(\"foobar\", Right (0, \'c\')),
	(\"barfoo\", Left \'d\')])@.

	If the route actually succeedes, @Just (Right (a, t))@ is returned, @Nothing@,
	if it fails.

	Note that this function is slower then @route@, so it should only be used,
	if the continuation possibilities of a route are needed.
-}
getBranch :: Ord s => t -> Route s t a -> [s] -> Maybe (Either [(s, Either t (a, t))] (a, t))
getBranch bt r bs = get bt bs (unRoute r) where
	get t [] (Pure a) = Just $ Right (a, t)
	get _ (x:xs) (Free (Capture _ t f)) = get t xs =<< f x
	get _ [] (Free (Capture es t f)) = Just . Left $
		mapMaybe (\s -> (s,) . getPure t <$> f s) es
	get t xs (Free (Choice as)) = interpret' $ mapMaybe (get t xs) as
	get _ _ _ = Nothing

	interpret' [] = Nothing
	interpret' xs = Just $ interpret [] xs
	
	interpret acc [] = Left . sortedNubBy ((==) `on` fst) $ sortBy (comparing fst) acc
	interpret acc (Left x : xs) = interpret (x ++ acc) xs
	interpret _ (Right a : _) = Right a

	getPure t (Pure a) = Right (a, t)
	getPure t _ = Left t

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True a = Just a
boolToMaybe False _ = Nothing

match' :: Eq s => [s] -> t -> s -> Route s t ()
match' es t s = capture es t (\s' -> boolToMaybe (s == s') ())

-- | A route which matches the segment @s@ tagged with @t@. If an other segment is provided,
--   the route fails.
match :: Eq s => t -> s -> Route s t ()
match t s = match' [s] t s

-- | Like 'match', but the generated segment is not shown by 'getBranch'.
matchHidden :: Eq s => t -> s -> Route s t ()
matchHidden t s = match' [] t s

-- | A route which matches any segment from the 'Set' @s@ tagged with @t@. It returns the
--   actually matched segment.
matchSet :: Ord s => t -> Set s -> Route s t s
matchSet t s = capture (S.elems s) t (\s' -> boolToMaybe (s' `S.member` s) s')

-- | A route which matches based of @f@. If for the given segment, @f s@ is
--   @Just a@, @a@ is returned, if it is @Nothing@, the route fails.
--   The captured segment is tagged with t.
--   The first parameter is a list of expected segments and used by 'getBranch'. This
--   list should not contain elements, for which the given function would return Nothing,
--   but may hide some elements, wich would actually match.
capture :: [s] -> t -> (s -> Maybe a) -> Route s t a
capture es t f = Route $ liftF (Capture es t f)

-- | Matches all segments for which the given function returns 'True'.
--   The result of the route is the matched segment.
captureBool :: [s] -> t -> (s -> Bool) -> Route s t s
captureBool es t f = capture es t $ \s -> boolToMaybe (f s) s

-- | Propes a list of routes and takes the first one, that does not fail.
--   Equivalent to 'msum', but more efficient.
choice :: [Route s t a] -> Route s t a
choice rs = Route . join $ liftF (Choice $ map unRoute rs)

-- | A route, which always fails. Equivalent to 'mzero'.
noRoute :: Route s t a
noRoute = Route $ liftF NoRoute
