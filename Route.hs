{-# LANGUAGE DeriveFunctor #-}
module Route where

import Control.Monad
import Control.Monad.Free
import Control.Applicative
import Data.Maybe

import Text.PrettyPrint

data Segment a = Match String a
	| Capture (String -> Maybe a)
	| Choice [a]
	| EOR a   -- ^ End Of Route (ha ha)
	| NoRoute
	deriving Functor

type Route = Free Segment

match :: String -> Route ()
match a = liftF (Match a ())

capture :: (String -> Maybe a) -> Route a
capture = liftF . Capture

choice :: [Route a] -> Route a
choice = join . liftF . Choice

eor :: Route ()
eor = liftF (EOR ())

noRoute :: Route a
noRoute = liftF NoRoute

runRoute :: Route a -> [String] -> Maybe (Either [(String, Maybe a)] a)
runRoute r s = go r s where
	go (Pure a) _ = Just $ Right a
	go (Free (EOR a)) [] = runRoute a []
	go a [] = Just . Left $ findEntries a
	go (Free (Match s a)) (x:xs) | s == x = runRoute a xs
	go (Free (Capture f)) (x:xs) = case f x of
		Nothing -> Nothing
		Just a -> runRoute a xs
	go (Free (Choice as)) xs = msum $ map (`runRoute` xs) as
	go _ _ = Nothing

getPure :: Route a -> Maybe a
getPure (Pure a) = Just a
getPure (Free (EOR a)) = getPure a
getPure _ = Nothing

findEntries :: Route a -> [(String, Maybe a)]
findEntries (Free (Match s a)) = [(s, getPure a)]
findEntries (Free (Choice a)) = concatMap findEntries a
findEntries _ = []

prettyRoute :: (Show a) => Route a -> Doc
prettyRoute (Pure a) = text "return" <+> text (show a)
prettyRoute (Free (Match p next)) = text "match"
	<+> doubleQuotes (text p) $+$ prettyRoute next
prettyRoute (Free (Capture f)) = text "capture <func>"
	<> text (show (fmap prettyRoute (f "")))
prettyRoute (Free (Choice cs)) = text "choice"
	<+> (cat $ map (\r -> text "do" <+> (nest 1 $ prettyRoute r)) cs)
prettyRoute (Free (EOR a)) = text "eor" $+$ prettyRoute a
prettyRoute (Free NoRoute) = text "no route"
