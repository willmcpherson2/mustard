module Mustard (Stages(..), mkStages) where

import Ast (Ast, mkAst)
import Bexpr (Bexpr, mkBexpr)
import Qualify (qualify)
import Sexpr (Sexpr, mkSexpr)
import Token (Token, mkTokens)
import TypeCheck (typeCheck)

data Stages = Stages
  { tokens :: [Token]
  , sexpr :: Sexpr
  , bexpr :: Bexpr
  , ast :: Ast
  , qualified :: Ast
  , typeChecked :: Ast
  }

mkStages :: String -> Stages
mkStages source =
  let
    tokens = mkTokens source
    sexpr = mkSexpr tokens
    bexpr = mkBexpr sexpr
    ast = mkAst bexpr
    qualified = qualify ast
    typeChecked = typeCheck qualified
  in Stages tokens sexpr bexpr ast qualified typeChecked
