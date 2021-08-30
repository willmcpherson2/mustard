module AstUtil (mapExprs, foldExprs) where

import Ast
import Data.Either (rights)

mapExprs :: (Expr -> Expr) -> Ast -> Ast
mapExprs f (Ast items) = Ast $ map (fmap (mapExprsItems f)) items

mapExprsItems :: (Expr -> Expr) -> Item -> Item
mapExprsItems f item = case item of
  ItemLet (Let name expr) -> ItemLet (Let name (mapExprsExpr f expr))
  _ -> item

mapExprsExpr :: (Expr -> Expr) -> Expr -> Expr
mapExprsExpr f expr =
  let Expr ty val = f expr
  in
    Expr ty $ case val of
      Right (ValLam (Lam id pat expr)) ->
        Right $ ValLam $ Lam id pat (mapExprsExpr f expr)
      Right (ValCase (Case caseExpr lams)) -> Right $ ValCase $ Case
        (mapExprsExpr f caseExpr)
        (map (fmap $ mapExprsLam f) lams)
      Right (ValApp (AppVal l r)) ->
        Right $ ValApp $ AppVal (mapExprsExpr f l) (mapExprsExpr f r)
      _ -> val

mapExprsLam :: (Expr -> Expr) -> Lam -> Lam
mapExprsLam f (Lam id pat expr) = Lam id pat (mapExprsExpr f expr)

--------------------------------------------------------------------------------

foldExprs :: (Expr -> b -> b) -> b -> Ast -> b
foldExprs f y (Ast items) = foldExprsItems f y (rights items)

foldExprsItems :: (Expr -> b -> b) -> b -> [Item] -> b
foldExprsItems f y items = case items of
  [] -> y
  item : items -> foldExprsItem f (foldExprsItems f y items) item

foldExprsItem :: (Expr -> b -> b) -> b -> Item -> b
foldExprsItem f y item = case item of
  ItemLet (Let _ expr) -> foldExprsExpr f y expr
  _ -> y

foldExprsExpr :: (Expr -> b -> b) -> b -> Expr -> b
foldExprsExpr f y expr@(Expr _ val) = case val of
  Right (ValLam (Lam _ _ lamExpr)) -> f expr (foldExprsExpr f y lamExpr)
  Right (ValCase (Case caseExpr lams)) ->
    f expr (foldExprsExpr f (foldExprsLams f y (rights lams)) caseExpr)
  Right (ValApp (AppVal l r)) ->
    f expr (foldExprsExpr f (foldExprsExpr f y l) r)
  _ -> f expr y

foldExprsLams :: (Expr -> b -> b) -> b -> [Lam] -> b
foldExprsLams f y lams = case lams of
  [] -> y
  lam : lams -> foldExprsLam f (foldExprsLams f y lams) lam

foldExprsLam :: (Expr -> b -> b) -> b -> Lam -> b
foldExprsLam f y (Lam _ _ expr) = foldExprsExpr f y expr
