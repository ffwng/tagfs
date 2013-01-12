{-# LANGUAGE DeriveFunctor #-}
module Route where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Free
import Data.Maybe

data Segment a = Match String a | Capture (String -> Maybe a) | Choice [a] | NoRoute
	deriving Functor

type RouteT = FreeT Segment
type Route = RouteT Identity

match :: Monad m => String -> RouteT m ()
match a = liftF (Match a ())

capture :: Monad m => (String -> Maybe a) -> RouteT m a
capture = liftF . Capture

choice :: Monad m => [RouteT m a] -> RouteT m a
choice = join . liftF . Choice

noRoute :: Monad m => RouteT m a
noRoute = liftF NoRoute

runRouteT :: Monad m => RouteT m a -> [String] -> m (Maybe (Either (RouteT m a) a))
runRouteT route segments = runFreeT route >>= \r -> go r segments where
	go (Pure a) _ = return $ Just (Right a)
	go r' [] = return . Just . Left . FreeT $ return r'
	go (Free (Match s a)) (x:xs) | s == x = runRouteT a xs
	go (Free (Capture f)) (x:xs) = case f x of
		Nothing -> return Nothing
		Just a -> runRouteT a xs
	go (Free (Choice as)) xs = firstJust $ map (`runRouteT` xs) as
	go _ _ = return Nothing

runRoute :: Route a -> [String] -> Maybe (Either (Route a) a)
runRoute r = runIdentity . runRouteT r

getSegments :: Route a -> Maybe [(String, Route a)]
getSegments r = go . runIdentity $ runFreeT r where
	go (Pure _) = Nothing
	go (Free NoRoute) = Nothing
	go (Free (Match s r)) = Just [(s, r)]
	go (Free (Choice as)) = Just . concat . catMaybes $ map getSegments as
	go _ = Just []

getLeafT :: Monad m => RouteT m a -> m (Maybe a)
getLeafT r = do
	res <- runRouteT r []
	case res of
		Nothing -> return Nothing
		Just (Left _) -> return Nothing
		Just (Right a) -> return (Just a)

getLeaf :: Route a -> Maybe a
getLeaf = runIdentity . getLeafT

firstJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJust [] = return Nothing
firstJust (x:xs) = x >>= \x' -> case x' of
	Nothing -> firstJust xs
	Just a -> return (Just a)
