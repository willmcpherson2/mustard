module Resolve
  ( resolve
  , resolveMaybe
  , Ref(..)
  , binderType
  , ctorType
  , patType
  ) where

import Ast
  ( AppPat(AppPat)
  , AppVal(AppVal)
  , Ast(Ast)
  , Case(Case)
  , Ctor(Ctor)
  , Def(Def)
  , Expr(Expr)
  , Fun(Fun)
  , Item(ItemDef, ItemLet)
  , Lam(Lam)
  , Let(Let)
  , Lit(LitUnit)
  , Pat(PatApp, PatBinder, PatCtor, PatLit)
  , Type(TypeFun, TypeName, TypeUnit)
  , Val(ValApp, ValCase, ValLam)
  )
import AstUtil (firstJustPat, inPat, lastJustPat)
import Control.Applicative ((<|>))
import Data.Either (rights)
import Data.List.Extra (firstJust)
import Data.Maybe (fromJust)
import Error (Fallible)
import Sexpr (Part(Anon), Path(Path))

data Ref
  = RefLet Let
  | RefType Def
  | RefCtor Def Ctor
  | RefLam (Fallible Type) Lam
  | RefCase (Fallible Type) Case Lam

resolve :: Ast -> Path -> Ref
resolve ast path = fromJust $ resolveMaybe ast path

resolveMaybe :: Ast -> Path -> Maybe Ref
resolveMaybe (Ast items) path = firstJust (resolveItem path) (rights items)

resolveItem :: Path -> Item -> Maybe Ref
resolveItem path item = case path of
  Path [] name -> case item of
    ItemLet le@(Let (Right letName) _) ->
      if name == letName then Just $ RefLet le else Nothing
    ItemDef def@(Def (Right defName) _) ->
      if name == defName then Just $ RefType def else Nothing
    _ -> Nothing
  Path (part : parts) name -> case resolveItem (Path [] part) item of
    Just (RefLet (Let _ expr)) -> resolveExpr (Path parts name) expr
    Just (RefType def@(Def _ ctors)) ->
      resolveCtors (Path parts name) def (rights ctors)
    _ -> Nothing

resolveExpr :: Path -> Expr -> Maybe Ref
resolveExpr path expr@(Expr ty val) = case path of
  Path [] name -> case val of
    Right (ValLam lam@(Lam id _ _)) ->
      if id == name then Just $ RefLam ty lam else Nothing
    Right (ValCase cas@(Case expr lams)) ->
      let
        inExpr = resolveExpr (Path [] name) expr
        inLam = RefCase ty cas <$> firstJust (resolveLam name) (rights lams)
      in inExpr <|> inLam
      where
        resolveLam name lam@(Lam id _ _) =
          if id == name then Just lam else Nothing
    Right (ValApp (AppVal l r)) ->
      resolveExpr (Path [] name) l <|> resolveExpr (Path [] name) r
    _ -> Nothing
  Path [Anon id] name -> case resolveExpr (Path [] (Anon id)) expr of
    ref@(Just (RefLam _ (Lam _ (Right pat) _))) ->
      if inPat name pat then ref else Nothing
    ref@(Just (RefCase _ _ (Lam _ (Right pat) _))) ->
      if inPat name pat then ref else Nothing
    _ -> Nothing
  Path (part : parts) name -> case resolveExpr (Path [] part) expr of
    Just (RefLam _ (Lam _ _ expr)) -> resolveExpr (Path parts name) expr
    Just (RefCase _ _ (Lam _ _ expr)) -> resolveExpr (Path parts name) expr
    _ -> Nothing

resolveCtors :: Path -> Def -> [Ctor] -> Maybe Ref
resolveCtors path def ctors = do
  Path [] name <- return path
  firstJust (resolveCtor name def) ctors
  where
    resolveCtor name def ctor = case ctor of
      Ctor (Right ctorName) _ ->
        if name == ctorName then Just $ RefCtor def ctor else Nothing
      _ -> Nothing

--------------------------------------------------------------------------------

binderType :: Ast -> Part -> Pat -> Maybe Type
binderType ast binder pat = do
  l <- lastJustPat (appOfBinder binder) pat
  Right ty : _ <- argsOfCtor ast l
  Just ty
  where
    appOfBinder binder pat = case pat of
      PatApp (AppPat (Right l) (Right (PatBinder r))) ->
        if binder == r then Just l else Nothing
      _ -> Nothing
    argsOfCtor ast pat = case pat of
      PatCtor (Right path) -> case resolve ast path of
        RefCtor _ (Ctor _ tys) -> Just tys
        _ -> undefined
      PatApp (AppPat (Right l) _) -> do
        Right _ : tys <- argsOfCtor ast l
        Just tys
      _ -> Nothing

--------------------------------------------------------------------------------

ctorType :: Ast -> Path -> Maybe Type
ctorType ast path = case resolve ast path of
  RefCtor (Def (Right defName) _) (Ctor _ tys) ->
    typesToFun (TypeName $ Right $ Path [] defName) tys
  _ -> undefined
  where
    typesToFun ret tys = case tys of
      [] -> Just ret
      Right ty : tys -> do
        tys <- typesToFun ret tys
        Just $ TypeFun $ Fun (Right ty) (Right tys)
      _ -> Nothing

--------------------------------------------------------------------------------

patType :: Ast -> Pat -> Maybe Type
patType ast pat = case pat of
  PatLit lit -> case lit of
    LitUnit -> Just TypeUnit
    _ -> undefined
  _ -> do
    ty <- firstJustPat (getCtorType ast) pat
    Just $ case ty of
      TypeFun (Fun _ (Right ty)) -> ty
      _ -> ty
  where
    getCtorType ast pat = case pat of
      PatCtor (Right path) -> ctorType ast path
      _ -> Nothing
