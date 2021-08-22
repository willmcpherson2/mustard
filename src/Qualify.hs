module Qualify (qualify) where

import Ast
  ( AppVal(AppVal)
  , Ast(Ast)
  , Case(Case)
  , Expr(Expr)
  , Item(ItemLet)
  , Lam(Lam)
  , Let(Let)
  , Val(ValApp, ValCase, ValLam)
  )
import Error (Fallible)

qualify :: Ast -> Ast
qualify (Ast items) = Ast $ map nameItem items

nameItem :: Fallible Item -> Fallible Item
nameItem item = case item of
  Right (ItemLet (Let path expr)) ->
    Right $ ItemLet $ Let path $ snd $ nameExpr 0 expr
  _ -> item

nameExpr :: Int -> Expr -> (Int, Expr)
nameExpr id expr = case expr of
  Expr ty (Right val) ->
    let (id', val') = nameVal id val in (id', Expr ty $ Right val')
  _ -> (id, expr)

nameVal :: Int -> Val -> (Int, Val)
nameVal id val = case val of
  ValLam lam -> let (id', lam') = nameLam id lam in (id', ValLam lam')
  ValCase (Case expr lams) ->
    let
      (id', expr') = nameExpr id expr
      (id'', lams') = nameLams id' lams
    in (id'', ValCase $ Case expr' lams')
  ValApp (AppVal l r) ->
    let
      (id', l') = nameExpr id l
      (id'', r') = nameExpr id' r
    in (id'', ValApp $ AppVal l' r')
  _ -> (id, val)

nameLams :: Int -> [Fallible Lam] -> (Int, [Fallible Lam])
nameLams id = foldr name (id, [])
  where
    name lam (id, lams) = case lam of
      Right lam ->
        let (id', lam') = nameLam id lam in (id', Right lam' : lams)
      Left lam -> (id, Left lam : lams)

nameLam :: Int -> Lam -> (Int, Lam)
nameLam id (Lam _ pat expr) =
  let (id', expr') = nameExpr (id + 1) expr in (id', Lam id pat expr')
