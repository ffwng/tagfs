{-# LANGUAGE DeriveFunctor #-}
module Route where

import ReEval

import Control.Monad
import Control.Monad.Free
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

newtype ReEvalSegment t a = ReEvalSegment {
	getSegment :: ReEval (Segment t a)
} deriving Functor

type Route t = Free (ReEvalSegment t)

new_ :: (b -> Segment t a) -> b -> ReEvalSegment t a
new_ f b = ReEvalSegment $ newReEval f b

read_ :: ReEvalSegment t a -> Segment t a
read_ = readReEval . getSegment

reset_ :: ReEvalSegment t a -> IO ()
reset_ = resetReEval . getSegment

match :: String -> Route t ()
match = liftF . new_ (\a -> Match a ())

capture :: (String -> Maybe a) -> Route t a
capture = liftF . new_ Capture

captureBool :: (String -> Bool) -> Route t String
captureBool f = capture (\a -> if f a then Just a else Nothing)

choice :: [Route t a] -> Route t a
choice = join . liftF . new_ Choice

tag :: t -> Route t ()
tag = liftF . new_ (\t -> Tag t ())

noRoute :: Route t a
noRoute = liftF $ new_ (\() -> NoRoute) ()

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
{-foldRoute (Free (Match s a)) = Free (Match s (foldRoute a))
foldRoute (Free (Tag t a)) = Free (Tag t (foldRoute a))
foldRoute (Free (Choice as)) = foldChoice' as-}
foldRoute r = r

{-foldChoice' as = case map compose . groupBy sameMatch $ sortBy comp (flatten as) of
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
		flatten (x:xs) = x : flatten xs-}

routeToEnd :: Route t a -> [String] -> Maybe (Either (Maybe t, Route t a) a)
routeToEnd r seg = go n seg r where
	n = Nothing
	go _ _ (Pure a) = Just (Right a)
	go t [] a = Just (Left (t, a))
	go t seg (Free f) = go' t seg (read_ f)

	go' :: Maybe t -> [String] -> Segment t (Route t a)
		-> Maybe (Either (Maybe t, Route t a) a)
	go' _ xs (Tag t a) = go (Just t) xs a
	go' _ (x:xs) (Match s a) | s == x = go n xs a
	go' _ (x:xs) (Capture f) = f x >>= go n xs
	go' _ xs (Choice as) = find xs as --msum $ map (go n xs) as
	go' _ _ _ = Nothing

	find xs = unsafePerformIO . find' where
		find' [] = return Nothing
		find' (a:as) = do
			let res = go n xs a
			if isJust res then do
				mapM reset_' as
				return res
			else do
				reset_' a
				find' as

	reset_' :: Route t a -> IO ()
	reset_' (Pure _) = return ()
	reset_' (Free f) = reset_ f

getPure :: Route t a -> Maybe a
getPure r = go r where
	go (Pure a) = Just a
	go (Free f) = case read_ f of
		Choice as -> msum $ map go as
		_ -> Nothing

findEntriesMap :: Route t a -> Maybe (Map String (Maybe a))
findEntriesMap (Free f) = case read_ f of
	Match s a -> Just $ M.singleton s (getPure a)
	Choice a -> Just . M.unions . catMaybes $ map findEntriesMap a
	Tag _ a -> findEntriesMap a
	_ -> Nothing
findEntriesMap _ = Nothing

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
