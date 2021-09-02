module Qualify (qualify) where

import Ast
  ( Ast
  , Case(Case)
  , Expr(Expr)
  , Lam(Lam)
  , Pat(PatCtor)
  , Type(TypeName)
  , Val(ValCase, ValCtor, ValLam, ValName)
  )
import AstUtil
  ( foldExprs
  , foldLams
  , inPat
  , mapExprs
  , mapExprsExpr
  , mapPats
  , mapScopes
  , mapTypes
  )
import Error (Error(InvalidPath), Fallible)
import Resolve (resolveMaybe)
import Sexpr (Part(Anon), Path(Path))

qualify :: Ast -> Ast
qualify = validate . qualifyNames . nameScopes

--------------------------------------------------------------------------------

nameScopes :: Ast -> Ast
nameScopes = fst . foldExprs nameLam 0
  where
    nameLam expr n = case expr of
      Expr ty (Right (ValLam (Lam _ pat expr))) ->
        (Expr ty $ Right $ ValLam $ Lam (Anon n) pat expr, n + 1)
      Expr ty (Right (ValCase (Case expr lams))) ->
        let (lams', n') = foldLams nameCaseLam n lams
        in (Expr ty $ Right $ ValCase $ Case expr lams', n')
      _ -> (expr, n)
    nameCaseLam (Lam _ pat expr) n = (Lam (Anon n) pat expr, n + 1)

--------------------------------------------------------------------------------

qualifyNames :: Ast -> Ast
qualifyNames = mapScopes qualifyScope

qualifyScope :: [Part] -> Lam -> Lam
qualifyScope qualifier (Lam id pat expr) = Lam id pat $ case pat of
  Right pat -> mapExprsExpr (qualifyName qualifier (`inPat` pat)) expr
  _ -> expr

qualifyName :: [Part] -> (Part -> Bool) -> Expr -> Expr
qualifyName qualifier inPat (Expr ty val) = Expr ty $ case val of
  Right (ValName (Right (Path valQualifier name))) ->
    let valQualifier' = if inPat name then qualifier else valQualifier
    in Right $ ValName $ Right $ Path valQualifier' name
  _ -> val

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
