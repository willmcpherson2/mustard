module Bexpr (Bexpr(..), mkBexpr) where

import Error (Error(UnexpectedOp))
import Sexpr (Atom(..), Op(..), Sexpr, Symbol(..))
import qualified Sexpr (Sexpr(..))

data Bexpr = Leaf Atom | Branch Op Bexpr Bexpr | Error Error
  deriving (Show)

mkBexpr :: Sexpr -> Bexpr
mkBexpr sexpr = case sexpr of
  Sexpr.Leaf leaf -> case leaf of
    Op _ -> Error UnexpectedOp
    Atom atom -> Leaf atom
  Sexpr.Branch branch -> case branch of
    [] -> Leaf Unit
    [sexpr] -> mkBexpr sexpr
    [l, r] -> Branch App (mkBexpr l) (mkBexpr r)
    l : Sexpr.Leaf (Op op) : r ->
      Branch op (mkBexpr l) (mkBexpr (Sexpr.Branch r))
    l : r : rest -> mkBexpr (Sexpr.Branch (Sexpr.Branch [l, r] : rest))
