{-# LANGUAGE DeriveFunctor #-}
module Route where

import ReEval

import Control.Monad
import Control.Monad.Trans.Free
import Control.Applicative
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import System.IO.Unsafe

import Text.PrettyPrint

data Segment t a = Match String a
	| Capture (String -> Maybe a)
	| Choice [a]
	| Tag t a
	| NoRoute
	deriving Functor

type Route t = FreeT (Segment t) ReEval
type RouteSegment t a = FreeF (Segment t) a (Route t a)

eval :: Route t a -> RouteSegment t a
eval = readReEval . runFreeT

uneval :: RouteSegment t a -> Route t a
uneval = FreeT . return

match :: String -> Route t ()
match a = liftF $ Match a ()

capture :: (String -> Maybe a) -> Route t a
capture = liftF . Capture

captureBool :: (String -> Bool) -> Route t String
captureBool f = capture (\a -> if f a then Just a else Nothing)

choice :: (Eq t, Ord t) => [Route t a] -> Route t a
choice = foldChoice

rawChoice :: [Route t a] -> Route t a
rawChoice = join . liftF . Choice

tag :: t -> Route t ()
tag t = liftF $ Tag t ()

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

foldChoice :: (Eq t, Ord t) => [Route t a] -> Route t a
foldChoice rs = case map compose . groupBy same . sortBy comp . flatten $ map eval rs of
	[] -> noRoute
	[x] -> x
	xs -> rawChoice xs
	where
		same (Free (Match s1 _)) (Free (Match s2 _)) = s1 == s2
		same (Free (Tag t1 _)) (Free (Tag t2 _)) = t1 == t2
		same _ _ = False

		-- groups tags and matches
		comp (Free (Tag t1 _)) (Free (Tag t2 _)) = compare t1 t2
		comp (Free (Tag _ _)) _ = LT
		comp (Free (Match s1 _)) (Free (Match s2 _)) = compare s1 s2
		comp (Free (Match _ _)) (Free (Tag _ _)) = GT
		comp (Free (Match _ _)) _ = LT
		comp _ _ = EQ

		compose :: [RouteSegment t a] -> Route t a
		compose [] = noRoute
		compose [x] = uneval x
		compose xs@(Free (Match p _) : _) = uneval $ Free (Match p (rawChoice
			[ n | (Free (Match _ n)) <- xs]))
		compose xs@(Free (Tag t _) : _) = uneval $ Free (Tag t (rawChoice
			[ n | (Free (Tag _ n)) <- xs]))
		compose _ = error "compose: assertion failed"
		
		flatten :: [RouteSegment t a] -> [RouteSegment t a]
		flatten [] = []
		flatten ((Free (Choice as)):xs) = flatten (map eval as) ++ flatten xs
		flatten (x:xs) = x : flatten xs

routeToEnd :: Route t a -> [String] -> Maybe (Either (Maybe t, Route t a) a)
routeToEnd r seg = go n seg $ eval r where
	n = Nothing

	go :: Maybe t -> [String] -> RouteSegment t a -> Maybe (Either (Maybe t, Route t a) a)
	go _ _ (Pure a) = Just (Right a)
	go _ xs (Free (Tag t a)) = go (Just t) xs $ eval a
	go t [] a = Just (Left (t, uneval a))
	go _ (x:xs) (Free (Match s a)) | s == x = go n xs $ eval a
	go _ (x:xs) (Free (Capture f)) = f x >>= go n xs . eval
	go _ xs (Free (Choice as)) = unsafePerformIO $ find xs as
	go _ _ _ = n

	find :: [String] -> [Route t a] -> IO (Maybe (Either (Maybe t, Route t a) a))
	find _ [] = return Nothing
	find xs (a:as) = do
		let res = go n xs $ eval a
		if isJust res then do
			mapM (resetReEval . runFreeT) as
			return res
		else do
			resetReEval $ runFreeT a
			find xs as

getPure :: Route t a -> Maybe a
getPure r = go $ eval r where
	go (Pure a) = Just a
	go (Free f) = case f of
		Choice as -> msum $ map getPure as
		_ -> Nothing

findEntriesMap :: Route t a -> Maybe (Map String (Maybe a))
findEntriesMap r = go $ eval r where
	go (Free (Match s a)) = Just $ M.singleton s (getPure a)
	go (Free (Choice a)) = Just . M.unions . catMaybes $ map findEntriesMap a
	go (Free (Tag _ a)) = findEntriesMap a
	go _ = Nothing

findEntries :: Route t a -> Maybe [(String, Maybe a)]
findEntries r = M.toList <$> findEntriesMap r

{-prettyRoute :: (Show t, Show a) => Int -> Route t a -> Doc
prettyRoute _ (Pure a) = text "return" <+> text (show a)
prettyRoute 0 _ = text ""
prettyRoute n (Free (Match p next)) = text "match"
	<+> doubleQuotes (text p) $+$ prettyRoute (n-1) next
prettyRoute n (Free (Capture f)) = text "capture <func>"
	<> text (show (fmap (prettyRoute (n-1)) (f "")))
prettyRoute n (Free (Choice cs)) = text "choice"
	<+> (cat $ map (\r -> text "do" <+> (nest 1 $ prettyRoute (n-1) r)) cs)
prettyRoute n (Free (Tag t a)) = text "tag" <+> text (show t) $+$ prettyRoute (n-1) a
prettyRoute n (Free NoRoute) = text "no route"-}
