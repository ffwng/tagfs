{-# LANGUAGE DeriveFunctor #-}
module Route where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.List

import Text.PrettyPrint

data Segment t a = Match String a
	| Capture (String -> Maybe a)
	| Choice [a]
	| Tag t a
	| NoRoute
	deriving Functor

type Route t = Free (Segment t)

match :: String -> Route t ()
match a = liftF (Match a ())

capture :: (String -> Maybe a) -> Route t a
capture = liftF . Capture

captureBool :: (String -> Bool) -> Route t String
captureBool f = capture (\a -> if f a then Just a else Nothing)

choice :: [Route t a] -> Route t a
choice = join . liftF . Choice

tag :: t -> Route t ()
tag t = liftF (Tag t ())

noRoute :: Route t a
noRoute = liftF NoRoute

runRoute :: Route t a -> [String] -> Maybe (Either (Maybe t) a)
runRoute r s = case routeToEnd r s of
	Nothing -> Nothing
	Just (Left t) -> Just (Left (fst t))
	Just (Right a) -> Just (Right a)

runTag :: Route t a -> [String] -> Maybe (Maybe (Maybe t, Route t a))
runTag r s = case routeToEnd r s of
	Nothing -> Nothing
	Just (Left e) -> Just (Just e)
	_ -> Just Nothing

getRestSegments :: Route t a -> Maybe [(FilePath, Maybe a)]
getRestSegments = findEntries

foldRoute :: (Eq t, Ord t) => Route t a -> Route t a
foldRoute (Free (Match s a)) = Free (Match s (foldRoute a))
foldRoute (Free (Tag t a)) = Free (Tag t (foldRoute a))
foldRoute (Free (Choice as)) = foldChoice' as
foldRoute r = r

foldChoice' as = case map compose . groupBy sameMatch $ sortBy comp (flatten as) of
	[] -> noRoute
	[x] -> x
	xs -> choice xs
	where
		sameMatch (Free (Match s1 _)) (Free (Match s2 _)) = s1 == s2
		sameMatch (Free (Tag t1 _)) (Free (Tag t2 _)) = t1 == t2
		sameMatch _ _ = False

		-- groups tags and matches
		comp (Free (Tag t1 _)) (Free (Tag t2 _)) = compare t1 t2
		comp (Free (Tag _ _)) _ = LT
		comp (Free (Match s1 _)) (Free (Match s2 _)) = compare s1 s2
		comp (Free (Match _ _)) (Free (Tag _ _)) = GT
		comp (Free (Match _ _)) _ = LT
		comp _ _ = EQ

		compose []  = noRoute
		compose [x] = foldRoute x
		compose xs@(Free (Match p _) : _) = Free (Match p (foldChoice'
			[ next | (Free (Match _ next)) <- xs]))
		compose xs@(Free (Tag t _) : _) = Free (Tag t (foldChoice'
			[ next | (Free (Tag _ next)) <- xs]))
		compose _ = error "compose: assertion failed"

		flatten :: [Route t a] -> [Route t a]
		flatten [] = []
		flatten ((Free (Choice as)):xs) = flatten as ++ flatten xs
		flatten (x:xs) = x : flatten xs

routeToEnd :: Route t a -> [String] -> Maybe (Either (Maybe t, Route t a) a)
routeToEnd = flip (go n) where
	n = Nothing
	go :: Maybe t -> [String] -> Route t a -> Maybe (Either (Maybe t, Route t a) a)
	go _ _ (Pure a) = Just (Right a)
	go _ xs (Free (Tag t a)) = go (Just t) xs a
	go t [] a = Just (Left (t, a))
	go _ (x:xs) (Free (Match s a)) | s == x = go n xs a
	go _ (x:xs) (Free (Capture f)) = f x >>= go n xs
	go _ xs (Free (Choice as)) = msum $ map (go n xs) as
	go _ _ _ = Nothing

getPure :: Route t a -> Maybe a
getPure = go where
	go (Pure a) = Just a
	go (Free (Choice as)) = msum $ map go as
	go _ = Nothing

findEntriesMap :: Route t a -> Maybe (Map String (Maybe a))
findEntriesMap (Free (Match s a)) = Just $ M.singleton s (getPure a)
findEntriesMap (Free (Choice a)) = Just . M.unions . catMaybes $ map findEntriesMap a
findEntriesMap (Free (Tag _ a)) = findEntriesMap a
findEntriesMap _ = Nothing

findEntries :: Route t a -> Maybe [(String, Maybe a)]
findEntries r = M.toList <$> findEntriesMap r

prettyRoute :: (Show t, Show a) => Int -> Route t a -> Doc
prettyRoute _ (Pure a) = text "return" <+> text (show a)
prettyRoute 0 _ = text ""
prettyRoute n (Free (Match p next)) = text "match"
	<+> doubleQuotes (text p) $+$ prettyRoute (n-1) next
prettyRoute n (Free (Capture f)) = text "capture <func>"
	<> text (show (fmap (prettyRoute (n-1)) (f "")))
prettyRoute n (Free (Choice cs)) = text "choice"
	<+> (cat $ map (\r -> text "do" <+> (nest 1 $ prettyRoute (n-1) r)) cs)
prettyRoute n (Free (Tag t a)) = text "tag" <+> text (show t) $+$ prettyRoute (n-1) a
prettyRoute n (Free NoRoute) = text "no route"
