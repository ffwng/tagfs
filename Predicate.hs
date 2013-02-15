{-# LANGUAGE DeriveFunctor #-}
module Predicate (
	Condition(..), ECondition(..),
	Var, Val(..), Op(..),
	Tree,
	parse, interpret, eval,
	eval', evalString
) where

import Data.Functor
import Control.Monad.Free

import Predicate.Parser (Var, Val, Op, Expr)
import qualified Predicate.Parser as P

data OpBranch a = And a a | Or a a | Not a
	deriving Functor

type Tree = Free OpBranch

data Condition = Exists Var | Is Op Var Val

data ECondition = Cond Condition | Const Bool

parse :: String -> Maybe (Tree ECondition)
parse s = convert <$> P.parseString s

convert :: Expr -> Tree ECondition
convert (P.Const b) = Pure (Const b)
convert (P.Leaf v) = Pure (Cond (Exists v))
convert (P.LeafOp o v1 v2) = Pure (Cond (Is o v1 v2))
convert (P.Not e) = Free (Not (convert e))
convert (P.Branch P.And a b) = Free (And (convert a) (convert b))
convert (P.Branch P.Or a b) = Free (Or (convert a) (convert b))

interpret :: (Condition -> Bool) -> Tree ECondition -> Tree Bool
interpret f = fmap (\c -> case c of
	Cond c' ->  f c'
	Const b -> b )

eval :: Tree Bool -> Bool
eval (Pure b) = b
eval (Free (And a b)) = eval a && eval b
eval (Free (Or a b)) = eval a || eval b
eval (Free (Not a)) = not $ eval a

eval' :: (Condition -> Bool) -> Tree ECondition -> Bool
eval' f = eval . interpret f

evalString :: (Condition -> Bool) -> String -> Maybe Bool
evalString f s = eval' f <$> parse s
