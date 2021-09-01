module AstUtil (mapExprs, foldExprs, mapTypes, mapPats, foldPat, inPat) where

import Ast
import Data.Either (fromRight, rights)
import Sexpr (Part)

mapPats :: (Pat -> Pat) -> Ast -> Ast
mapPats f = mapExprs (mapPatsExpr f)

mapPatsExpr :: (Pat -> Pat) -> Expr -> Expr
mapPatsExpr f (Expr ty val) = Expr ty $ case val of
  Right (ValLam lam) -> Right $ ValLam $ mapPatsLam f lam
  Right (ValCase (Case expr lams)) ->
    Right $ ValCase $ Case expr $ map (fmap $ mapPatsLam f) lams
  _ -> val

mapPatsLam :: (Pat -> Pat) -> Lam -> Lam
mapPatsLam f (Lam id pat expr) = Lam id (mapPatsPat f <$> pat) expr

mapPatsPat :: (Pat -> Pat) -> Pat -> Pat
mapPatsPat f pat = case f pat of
  PatApp (AppPat l r) ->
    PatApp $ AppPat (mapPatsPat f <$> l) (mapPatsPat f <$> r)
  pat -> pat

foldPat :: (Pat -> a -> a) -> a -> Pat -> a
foldPat f y pat = case pat of
  PatApp (AppPat l r) ->
    let
      r' = foldPat f y <$> r
      l' = foldPat f (fromRight y r') <$> l
      pat' = f pat (fromRight y l')
    in pat'
  _ -> f pat y

inPat :: Part -> Pat -> Bool
inPat name = foldPat (\x acc -> acc || binderMatch name x) False
  where
    binderMatch name pat = case pat of
      PatBinder patName -> name == patName
      _ -> False

--------------------------------------------------------------------------------

mapTypes :: (Type -> Type) -> Ast -> Ast
mapTypes f (Ast items) = Ast $ map (fmap (mapTypesItem f)) items

mapTypesItem :: (Type -> Type) -> Item -> Item
mapTypesItem f item = case item of
  ItemLet (Let name expr) -> ItemLet $ Let name (mapTypesExpr f expr)
  ItemDef def -> ItemDef $ mapTypesDef f def

mapTypesDef :: (Type -> Type) -> Def -> Def
mapTypesDef f (Def name ctors) = Def name (map (fmap (mapTypesCtor f)) ctors)

mapTypesCtor :: (Type -> Type) -> Ctor -> Ctor
mapTypesCtor f (Ctor name tys) = Ctor name (map (fmap f) tys)

mapTypesExpr :: (Type -> Type) -> Expr -> Expr
mapTypesExpr f = mapExprsExpr (apply f)
  where
    apply f (Expr ty val) = case ty of
      Right ty -> Expr (Right $ f ty) val
      _ -> Expr ty val

--------------------------------------------------------------------------------

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
