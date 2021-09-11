module AstUtil
  ( mapExprs
  , foldExprs
  , mapTypes
  , mapPats
  , foldPat
  , inPat
  , foldExprsLams
  , foldLams
  , mapExprsExpr
  , mapScopes
  , firstJustExprs
  , firstJustPat
  , lastJustPat
  , mapExprTypes
  ) where

import Ast
import Data.Either (fromRight)
import Data.Tuple.Extra
import Error (Fallible)
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

firstJustPat :: (Pat -> Maybe a) -> Pat -> Maybe a
firstJustPat f = foldPat (apply f) Nothing
  where
    apply f pat x = case x of
      Just x -> Just x
      _ -> f pat

lastJustPat :: (Pat -> Maybe a) -> Pat -> Maybe a
lastJustPat f = foldPat (apply f) Nothing
  where
    apply f pat x = case f pat of
      Just x' -> Just x'
      _ -> x

inPat :: Part -> Pat -> Bool
inPat name = foldPat (\x acc -> acc || binderMatch name x) False
  where
    binderMatch name pat = case pat of
      PatBinder patName -> name == patName
      _ -> False

--------------------------------------------------------------------------------

mapTypes :: (Fallible Type -> Fallible Type) -> Ast -> Ast
mapTypes f (Ast items) = Ast $ map (fmap (mapTypesItem f)) items

mapTypesItem :: (Fallible Type -> Fallible Type) -> Item -> Item
mapTypesItem f item = case item of
  ItemLet (Let name expr) -> ItemLet $ Let name (mapTypesExpr f expr)
  ItemDef def -> ItemDef $ mapTypesDef f def

mapTypesDef :: (Fallible Type -> Fallible Type) -> Def -> Def
mapTypesDef f (Def name ctors) = Def name (map (fmap (mapTypesCtor f)) ctors)

mapTypesCtor :: (Fallible Type -> Fallible Type) -> Ctor -> Ctor
mapTypesCtor f (Ctor name tys) = Ctor name (map (mapTypesType f) tys)

mapTypesExpr :: (Fallible Type -> Fallible Type) -> Expr -> Expr
mapTypesExpr f = mapExprsExpr (apply f)
  where apply f (Expr ty val) = Expr (f ty) val

mapTypesType
  :: (Fallible Type -> Fallible Type) -> Fallible Type -> Fallible Type
mapTypesType f ty = case f ty of
  Right (TypeFun (Fun l r)) ->
    Right $ TypeFun $ Fun (mapTypesType f l) (mapTypesType f r)
  ty -> ty

mapExprTypes :: (Fallible Type -> Fallible Type) -> Ast -> Ast
mapExprTypes f = mapExprs (apply f)
  where apply f (Expr ty val) = Expr (mapTypesType f ty) val

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

mapScopes :: ([Part] -> Lam -> Lam) -> Ast -> Ast
mapScopes f (Ast items) = Ast $ map (fmap $ mapScopesItem f) items

mapScopesItem :: ([Part] -> Lam -> Lam) -> Item -> Item
mapScopesItem f item = case item of
  ItemLet (Let (Right name) expr) ->
    ItemLet $ Let (Right name) (mapScopesExpr f [name] expr)
  _ -> item

mapScopesExpr :: ([Part] -> Lam -> Lam) -> [Part] -> Expr -> Expr
mapScopesExpr f qualifier (Expr ty val) = Expr ty $ case val of
  Right (ValLam lam) -> Right $ ValLam $ mapScopesLam f qualifier lam
  Right (ValCase (Case caseExpr lams)) -> Right $ ValCase $ Case
    (mapScopesExpr f qualifier caseExpr)
    (map (fmap $ mapScopesLam f qualifier) lams)
  Right (ValApp (AppVal l r)) -> Right $ ValApp $ AppVal
    (mapScopesExpr f qualifier l)
    (mapScopesExpr f qualifier r)
  _ -> val

mapScopesLam :: ([Part] -> Lam -> Lam) -> [Part] -> Lam -> Lam
mapScopesLam f qualifier lam =
  let
    qualifier' = qualifier ++ [id]
    Lam id pat expr = f qualifier' lam
  in Lam id pat $ mapScopesExpr f qualifier' expr

--------------------------------------------------------------------------------

foldExprs :: (Expr -> a -> (Expr, a)) -> a -> Ast -> (Ast, a)
foldExprs f x (Ast items) =
  let (items', x') = foldExprsItems f x items in (Ast items', x')

foldExprsItems
  :: (Expr -> a -> (Expr, a)) -> a -> [Fallible Item] -> ([Fallible Item], a)
foldExprsItems f x items = case items of
  [] -> ([], x)
  item : items ->
    let
      (item', x') = foldExprsItem f x item
      (items', x'') = foldExprsItems f x' items
    in (item' : items', x'')

foldExprsItem
  :: (Expr -> a -> (Expr, a)) -> a -> Fallible Item -> (Fallible Item, a)
foldExprsItem f x item = case item of
  Right (ItemLet (Let name expr)) ->
    let (expr', x') = foldExprsExpr f x expr
    in (Right $ ItemLet $ Let name expr', x')
  _ -> (item, x)

foldExprsExpr :: (Expr -> a -> (Expr, a)) -> a -> Expr -> (Expr, a)
foldExprsExpr f x expr = case f expr x of
  (Expr ty (Right (ValApp (AppVal l r))), x') ->
    let
      (l', x'') = foldExprsExpr f x' l
      (r', x''') = foldExprsExpr f x'' r
    in (Expr ty (Right $ ValApp $ AppVal l' r'), x''')
  (Expr ty (Right (ValLam (Lam id pat lamExpr))), x') ->
    let (lamExpr', x'') = foldExprsExpr f x' lamExpr
    in (Expr ty (Right $ ValLam $ Lam id pat lamExpr'), x'')
  (Expr ty (Right (ValCase (Case caseExpr lams))), x') ->
    let
      (caseExpr', x'') = foldExprsExpr f x' caseExpr
      (lams', x''') = foldExprsLams f x'' lams
    in (Expr ty (Right $ ValCase $ Case caseExpr' lams'), x''')
  _ -> f expr x

foldExprsLams
  :: (Expr -> a -> (Expr, a)) -> a -> [Fallible Lam] -> ([Fallible Lam], a)
foldExprsLams f x lams = case lams of
  [] -> ([], x)
  lam : lams ->
    let
      (lam', x') = foldExprsLam f x lam
      (lams', x'') = foldExprsLams f x' lams
    in (lam' : lams', x'')

foldExprsLam
  :: (Expr -> a -> (Expr, a)) -> a -> Fallible Lam -> (Fallible Lam, a)
foldExprsLam f x lam = case lam of
  Right (Lam id pat expr) ->
    let (expr', x') = f expr x in (Right $ Lam id pat expr', x')
  _ -> (lam, x)

--------------------------------------------------------------------------------

foldLams
  :: (Lam -> a -> (Lam, a)) -> a -> [Fallible Lam] -> ([Fallible Lam], a)
foldLams f x lams = case lams of
  [] -> ([], x)
  lam : lams ->
    let
      (lam', x') = case lam of
        Right lam -> first Right (f lam x)
        Left _ -> (lam, x)
      (lams', x'') = foldLams f x' lams
    in (lam' : lams', x'')

--------------------------------------------------------------------------------

firstJustExprs :: (Expr -> Maybe a) -> Ast -> Maybe a
firstJustExprs f = snd . foldExprs (apply f) Nothing
  where
    apply f expr result = case result of
      Just result -> (expr, Just result)
      _ -> (expr, f expr)
