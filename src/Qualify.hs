module Qualify (qualify) where

import Ast
  ( AppVal(AppVal)
  , Ast(Ast)
  , Case(Case)
  , Expr(Expr)
  , Item(ItemLet)
  , Lam(Lam)
  , Let(Let)
  , Pat(PatBinder, PatCtor)
  , Type(TypeName)
  , Val(ValApp, ValCase, ValCtor, ValLam, ValName)
  )
import AstUtil (foldPat, inPat, mapExprs, mapPats, mapTypes)
import Data.Either (fromRight)
import Error (Error(InvalidPath), Fallible)
import Resolve (resolveMaybe)
import Sexpr (Part(Anon), Path(Path))

qualify :: Ast -> Ast
qualify = validate . qualifyScopes . nameScopes

--------------------------------------------------------------------------------

nameScopes :: Ast -> Ast
nameScopes (Ast items) = Ast $ snd $ foldr nameScopesItem (0, []) items

nameScopesItem
  :: Fallible Item -> (Int, [Fallible Item]) -> (Int, [Fallible Item])
nameScopesItem item (id, items) = case item of
  Right (ItemLet (Let path expr)) ->
    let (id', expr') = nameScopesExpr id expr
    in (id', Right (ItemLet $ Let path expr') : items)
  _ -> (id, item : items)

nameScopesExpr :: Int -> Expr -> (Int, Expr)
nameScopesExpr id expr = case expr of
  Expr ty (Right val) ->
    let (id', val') = nameScopesVal id val in (id', Expr ty $ Right val')
  _ -> (id, expr)

nameScopesVal :: Int -> Val -> (Int, Val)
nameScopesVal id val = case val of
  ValLam lam -> let (id', lam') = nameScopesLam id lam in (id', ValLam lam')
  ValCase (Case expr lams) ->
    let
      (id', expr') = nameScopesExpr id expr
      (id'', lams') = nameScopesLams id' lams
    in (id'', ValCase $ Case expr' lams')
  ValApp (AppVal l r) ->
    let
      (id', l') = nameScopesExpr id l
      (id'', r') = nameScopesExpr id' r
    in (id'', ValApp $ AppVal l' r')
  _ -> (id, val)

nameScopesLams :: Int -> [Fallible Lam] -> (Int, [Fallible Lam])
nameScopesLams id = foldr name (id, [])
  where
    name lam (id, lams) = case lam of
      Right lam ->
        let (id', lam') = nameScopesLam id lam in (id', Right lam' : lams)
      Left lam -> (id, Left lam : lams)

nameScopesLam :: Int -> Lam -> (Int, Lam)
nameScopesLam id (Lam _ pat expr) =
  let (id', expr') = nameScopesExpr (id + 1) expr in (id', Lam id pat expr')

--------------------------------------------------------------------------------

qualifyScopes :: Ast -> Ast
qualifyScopes (Ast items) = Ast $ map (fmap qualifyScopesItem) items

qualifyScopesItem :: Item -> Item
qualifyScopesItem item = case item of
  ItemLet (Let (Right path) expr) ->
    ItemLet $ Let (Right path) (qualifyScopesExpr (Path [] path) expr)
  _ -> item

qualifyScopesExpr :: Path -> Expr -> Expr
qualifyScopesExpr path expr = case expr of
  Expr ty (Right val) -> Expr ty (Right $ qualifyScopesVal path val)
  _ -> expr

qualifyScopesVal :: Path -> Val -> Val
qualifyScopesVal path val = case val of
  ValLam lam -> ValLam $ qualifyNames path lam
  ValCase (Case expr lams) -> ValCase
    $ Case (qualifyScopesExpr path expr) (map (fmap $ qualifyNames path) lams)
  ValApp (AppVal l r) ->
    ValApp $ AppVal (qualifyScopesExpr path l) (qualifyScopesExpr path r)
  _ -> val

qualifyNames :: Path -> Lam -> Lam
qualifyNames (Path qualifier name) (Lam id pat expr) =
  let
    path = Path (qualifier ++ [name]) (Anon id)
    expr' = qualifyScopesExpr path expr
    expr'' = fromRight expr' (foldPat qualify expr' <$> pat)
  in Lam id pat expr''
  where
    qualify pat expr = case pat of
      PatBinder binder ->
        let path = Path (qualifier ++ [name] ++ [Anon id]) binder
        in qualifyNamesExpr path expr
      _ -> expr

qualifyNamesExpr :: Path -> Expr -> Expr
qualifyNamesExpr path expr = case expr of
  Expr ty (Right val) -> Expr ty (Right $ qualifyNamesVal path val)
  _ -> expr

qualifyNamesVal :: Path -> Val -> Val
qualifyNamesVal path@(Path _ name) val = case val of
  ValName valPath@(Right (Path [] valName)) ->
    ValName $ if name == valName then Right path else valPath
  ValLam lam -> ValLam $ qualifyNamesLam path lam
  ValCase (Case expr lams) -> ValCase $ Case
    (qualifyNamesExpr path expr)
    (map (fmap $ qualifyNamesLam path) lams)
  ValApp (AppVal l r) ->
    ValApp $ AppVal (qualifyNamesExpr path l) (qualifyNamesExpr path r)
  _ -> val

qualifyNamesLam :: Path -> Lam -> Lam
qualifyNamesLam path@(Path _ name) (Lam id pat expr) = Lam id pat $ case pat of
  Right pat -> if inPat name pat then expr else qualifyNamesExpr path expr
  _ -> qualifyNamesExpr path expr

--------------------------------------------------------------------------------

validate :: Ast -> Ast
validate ast =
  mapTypes (validateType ast)
    . mapExprs (validateExpr ast)
    . mapPats (validatePat ast)
    $ ast

validateExpr :: Ast -> Expr -> Expr
validateExpr ast (Expr ty val) = Expr ty $ case val of
  Right (ValName (Right path)) -> Right $ ValName $ validatePath ast path
  Right (ValCtor (Right path)) -> Right $ ValCtor $ validatePath ast path
  _ -> val

validatePat :: Ast -> Pat -> Pat
validatePat ast pat = case pat of
  PatCtor (Right path) -> PatCtor $ validatePath ast path
  _ -> pat

validateType :: Ast -> Type -> Type
validateType ast ty = case ty of
  TypeName (Right path) -> TypeName $ validatePath ast path
  _ -> ty

validatePath :: Ast -> Path -> Fallible Path
validatePath ast path = case resolveMaybe ast path of
  Just _ -> Right path
  _ -> Left InvalidPath
