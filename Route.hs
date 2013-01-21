module Route (
	Route,
	tag,
	match,
	capture,
	choice,
	noRoute,
	runRoute
) where

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

type Route s t a = StateT [s] (MaybeT (State ([s], Maybe t, [s]))) a

tagMaybe :: Maybe t -> Route s t ()
tagMaybe t = lift . lift $ modify (\(a, _, b) -> (a, t, b))

tag :: t -> Route s t ()
tag = tagMaybe . Just

clearTag :: Route s t ()
clearTag = tagMaybe Nothing

expect :: [s] -> Route s t ()
expect xs = lift . lift $ modify (\(a, b, _) -> (a, b, xs))

consume :: s -> Route s t ()
consume s = lift . lift $ modify (\(a, b, c) -> (s:a, b, c))

setConsumed :: [s] -> Route s t ()
setConsumed s = lift . lift $ modify (\(_, b, c) -> (s, b, c))

clearExpect :: Route s t ()
clearExpect = expect []

nextMaybe :: Route s t (Maybe s)
nextMaybe = do
	xs <- get
	case xs of
		p:ps -> do
			put ps
			return (Just p)
		_ -> noRoute

next :: Route s t s
next = maybe noRoute return =<< nextMaybe

match :: Eq s => s -> Route s t ()
match s = do
	expect [s]
	s' <- next
	clearExpect
	if s == s' then consume s >> return ()
	else noRoute

capture :: (s -> Maybe a) -> Route s t a
capture f = do
	clearExpect
	s <- next
	case f s of
		Just a -> consume s >> return a
		_ -> noRoute

captureBool :: (s -> Bool) -> Route s t s
captureBool f = capture (\s -> if f s then Just s else Nothing)

noRoute :: Route s t a
noRoute = mzero

choice :: Eq s => [Route s t a] -> Route s t a
choice rs = do
	s <- get
	let res = map (`runRoute` s) rs
	-- gather expected from all routes, tag and value from first route
	let third (_, _, c) = c
	let expected = concatMap (third . snd) res
	expect expected
	let consumed (r, (a, _, _)) = isJust r || not (null a)
	let succs = filter consumed res
	case succs of
		(r, (c, t, _)):_ -> do
			tagMaybe t
			setConsumed c
			case r of
				Just (a, s') -> do
					put s'
					return a
				_ -> noRoute
		_ -> noRoute

runRoute :: Route s t a -> [s] -> (Maybe (a, [s]), ([s], Maybe t, [s]))
runRoute r s = runIdentity . (`runStateT` ([], Nothing, [])) . runMaybeT $ runStateT r s
