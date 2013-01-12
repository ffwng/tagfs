{-# LANGUAGE DeriveFunctor #-}
module Route where

import Control.Monad
import Control.Monad.Free
import Data.Maybe

data Segment a = Match String a | Capture (String -> Maybe a) | Choice [a] | NoRoute
	deriving Functor

type Route = Free Segment

match :: String -> Route ()
match a = liftF (Match a ())

capture :: (String -> Maybe a) -> Route a
capture = liftF . Capture

choice :: [Route a] -> Route a
choice = join . liftF . Choice

noRoute :: Route a
noRoute = liftF NoRoute

runRoute :: Route a -> [String] -> Maybe (Either (Route a) a)
runRoute = go where
	go (Pure a) _ = Just (Right a)
	go r' [] = Just $ Left r'
	go (Free (Match s a)) (x:xs) | s == x = runRoute a xs
	go (Free (Capture f)) (x:xs) = case f x of
		Nothing -> Nothing
		Just a -> runRoute a xs
	go (Free (Choice as)) xs = msum $ map (`runRoute` xs) as
	go _ _ = Nothing

getSegments :: Route a -> Maybe [(String, Route a)]
getSegments = go where
	go (Pure _) = Nothing
	go (Free NoRoute) = Nothing
	go (Free (Match s r)) = Just [(s, r)]
	go (Free (Choice as)) = Just . concat . catMaybes $ map getSegments as
	go _ = Just []

getLeaf :: Route a -> Maybe a
getLeaf r = case r of
	Pure a -> Just a
	_ -> Nothing

