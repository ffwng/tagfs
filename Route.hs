module Route where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Applicative
import Control.Arrow

import Data.Monoid
import Data.Maybe

type Route s t a = StateT [s] (MaybeT (State (Maybe t, [s]))) a

tagMaybe :: Maybe t -> Route s t ()
tagMaybe = lift . lift . modify .first . const

tag :: t -> Route s t ()
tag = tagMaybe . Just

clearTag :: Route s t ()
clearTag = tagMaybe Nothing

expect :: [s] -> Route s t ()
expect xs = lift . lift . modify . second $ const xs

clearExpect :: Route s t ()
clearExpect = expect []

nextMaybe :: Route s t (Maybe s)
nextMaybe = do
	xs <- get
	case xs of
		p:ps -> put ps >> return (Just p)
		_ -> noRoute

next :: Route s t s
next = maybe noRoute return =<< nextMaybe

match :: Eq s => s -> Route s t ()
match s = do
	expect [s]
	s' <- next
	clearExpect
	if s == s' then return ()
	else noRoute

capture :: (s -> Maybe a) -> Route s t a
capture f = do
	clearExpect
	s <- next
	maybe noRoute return $ f s

captureBool :: (s -> Bool) -> Route s t s
captureBool f = capture (\s -> if f s then Just s else Nothing)

noRoute :: Route s t a
noRoute = mzero

choice :: Eq s => [Route s t a] -> Route s t a
choice rs = do
	s <- get
	let res = map (`runRoute` s) rs
	-- gather expected from all routes, tag and value from first route
	let expected = concatMap (snd . snd) res
	expect expected
	let consumed (r, (_, s')) = isJust r || s' /= s
	let succs = filter consumed res
	case succs of
		(Just (a, s'), (t, _)):_ -> do
			tagMaybe t
			put s'
			return a
		_ -> noRoute

runRoute :: Route s t a -> [s] -> (Maybe (a, [s]), (Maybe t, [s]))
runRoute r s = runIdentity . (`runStateT` (Nothing, [])) . runMaybeT $ runStateT r s
