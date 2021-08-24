module Sexpr
  ( Sexpr(..)
  , Op(..)
  , Atom(..)
  , Lit(..)
  , Symbol(..)
  , Name(..)
  , Path(..)
  , Part(..)
  , mkSexpr
  ) where

import Control.Monad (guard)
import Data.Char (isUpper)
import Data.Foldable (asum)
import Data.List.Split (splitOn)
import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)
import Error (Error(InvalidName))
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import Token (Token(..))

data Sexpr = Leaf Symbol | Branch [Sexpr]
  deriving (Show)

data Symbol
  = Op Op
  | Atom Atom
  deriving (Show)

data Atom
  = Name Name
  | Lit Lit
  | Unit
  deriving (Show)

data Name
  = Upper Path
  | Lower Path
  | NameError Error
  deriving (Show)

data Op
  = Dollar
  | Equal
  | Colon
  | Pipe
  | Arrow
  | Question
  | App
  deriving (Show, Eq, Ord)

data Lit
  = Int Int
  | Float Float
  deriving (Show)

data Path
  = Path [Part] Part
  | PathError Error
  deriving Show

data Part
  = Named String
  | Anon Int
  deriving (Show, Eq)

mkSexpr :: [Token] -> Sexpr
mkSexpr = Branch . snd . mk
  where
    mk tokens = case tokens of
      Open : tokens ->
        let
          (tokens', sexprs) = mk tokens
          branch = Branch sexprs
          (tokens'', sexprs') = mk tokens'
        in (tokens'', branch : sexprs')
      Other s : tokens ->
        let
          leaf = Leaf $ mkSymbol s
          (tokens', sexprs) = mk tokens
        in (tokens', leaf : sexprs)
      Close : tokens -> (tokens, [])
      _ : tokens -> let (tokens', sexprs) = mk tokens in (tokens', sexprs)
      [] -> ([], [])

mkSymbol :: String -> Symbol
mkSymbol s =
  let
    error = Atom $ Name $ NameError InvalidName
    maybe = asum $ map ($ s) [mkOp, mkLit, mkName]
  in fromMaybe error maybe

mkOp :: String -> Maybe Symbol
mkOp s = Op <$> lookup s (fromList ops)
  where
    ops =
      [ ("?", Question)
      , ("->", Arrow)
      , (":", Colon)
      , ("|", Pipe)
      , ("=", Equal)
      , ("$", Dollar)
      ]

mkLit :: String -> Maybe Symbol
mkLit s = asum $ map ($ s) [mkInt, mkFloat]

mkInt :: String -> Maybe Symbol
mkInt s = do
  int <- readMaybe s :: Maybe Int
  Just $ Atom $ Lit (Int int)

mkFloat :: String -> Maybe Symbol
mkFloat s = do
  float <- readMaybe s :: Maybe Float
  Just $ Atom $ Lit (Float float)

mkName :: String -> Maybe Symbol
mkName s = do
  let parts = splitOn "." s
  guard $ not (null parts) && not (any null parts)
  let
    qualifier = map Named (init parts)
    name = last parts
    ctor = if isUpper (head name) then Upper else Lower
  Just $ Atom $ Name $ ctor $ Path qualifier (Named name)
