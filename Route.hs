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
	-- | EOR a   -- ^ End Of Route (ha ha)
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

--eor :: Route ()
--eor = liftF (EOR ())

--isEOR :: Route Bool
--isEOR = choice [eor >> return True, return False]

noRoute :: Route t a
noRoute = liftF NoRoute

-- Nothing -> route failed
-- Just Nothing -> route succeeded, but had not finished
-- Just (Just x) -> route had finished with x
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

-- Nothing -> route failed
-- Just Nothing -> route succeeded, but leaved no rest segments
-- Just (Just e) -> route succeeded and leaved rest segments e
-- Note: a route may both finish and leave rest segments
-- (consider choice [eor >> return "finished", match "name" >> return "restsegment"]
getRestSegments :: Route t a -> [String] -> Maybe (Maybe [(FilePath, Maybe a)])
getRestSegments r s = fmap (>>= findEntries . snd) (runTag r s)

foldRoute :: Route t a -> Route t a
foldRoute (Free (Match s a)) = Free (Match s (foldRoute a))
foldRoute (Free (Choice as)) = foldChoice' as
foldRoute r = r

foldChoice' as = case map flatten $ groupBy sameMatch as of
	[] -> noRoute
	[x] -> x
	xs -> choice xs
	where
		sameMatch (Free (Match s1 _)) (Free (Match s2 _)) = s1 == s2
		sameMatch _ _ = False
		flatten []  = noRoute
		flatten [x] = foldRoute x
		flatten xs@(Free (Match p _) : _) = Free (Match p (foldChoice'
			[ next | (Free (Match _ next)) <- xs]))
		flatten _ = error "flatten: assertion failed"

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
findEntriesMap _ = Nothing

findEntries :: Route t a -> Maybe [(String, Maybe a)]
findEntries r = M.toList <$> findEntriesMap r

prettyRoute :: (Show t, Show a) => Route t a -> Doc
prettyRoute (Pure a) = text "return" <+> text (show a)
prettyRoute (Free (Match p next)) = text "match"
	<+> doubleQuotes (text p) $+$ prettyRoute next
prettyRoute (Free (Capture f)) = text "capture <func>"
	<> text (show (fmap prettyRoute (f "")))
prettyRoute (Free (Choice cs)) = text "choice"
	<+> (cat $ map (\r -> text "do" <+> (nest 1 $ prettyRoute r)) cs)
prettyRoute (Free (Tag t a)) = text "tag" <+> text (show t) $+$ prettyRoute a
prettyRoute (Free NoRoute) = text "no route"
