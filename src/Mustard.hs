module Mustard (Stages(..), mkStages) where

import Ast (Ast, mkAst)
import Bexpr (Bexpr, mkBexpr)
import Sexpr (Sexpr, mkSexpr)
import Token (Token, mkTokens)

data Stages = Stages
  { tokens :: [Token]
  , sexpr :: Sexpr
  , bexpr :: Bexpr
  , ast :: Ast
  }

mkStages :: String -> Stages
mkStages source =
  let
    tokens = mkTokens source
    sexpr = mkSexpr tokens
    bexpr = mkBexpr sexpr
    ast = mkAst bexpr
  in Stages tokens sexpr bexpr ast
