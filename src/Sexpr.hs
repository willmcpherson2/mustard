module Sexpr (Sexpr(..), Op(..), Atom(..), Lit(..), Symbol(..), mkSexpr) where

import Data.Foldable (asum)
import Data.List.Split (splitOn)
import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)
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
  = Name [String]
  | Lit Lit
  | Unit
  deriving (Show)

data Op
  = Dollar
  | ColonEqual
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

mkSexpr :: [Token] -> Sexpr
mkSexpr = Branch . snd . mk
  where
    mk (Open : tokens) =
      let
        (tokens', sexprs) = mk tokens
        branch = Branch sexprs
        (tokens'', sexprs') = mk tokens'
      in (tokens'', branch : sexprs')
    mk (Other s : tokens) =
      let
        leaf = Leaf $ mkSymbol s
        (tokens', sexprs) = mk tokens
      in (tokens', leaf : sexprs)
    mk (Close : tokens) = (tokens, [])
    mk (_ : tokens) = let (tokens', sexprs) = mk tokens in (tokens', sexprs)
    mk [] = ([], [])

mkSymbol :: String -> Symbol
mkSymbol s =
  let maybe = asum $ map ($ s) [mkOp, mkLit] in fromMaybe (mkName s) maybe

mkOp :: String -> Maybe Symbol
mkOp s = Op <$> lookup s (fromList ops)
  where
    ops =
      [ ("?", Question)
      , ("->", Arrow)
      , (":", Colon)
      , ("|", Pipe)
      , ("=", Equal)
      , (":=", ColonEqual)
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

mkName :: String -> Symbol
mkName = Atom . Name . splitOn "."
