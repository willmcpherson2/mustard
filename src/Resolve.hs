module Resolve (resolve, resolveMaybe) where

import Ast
  ( AppVal(AppVal)
  , Ast(Ast)
  , Case(Case)
  , Ctor(Ctor)
  , Def(Def)
  , Expr(Expr)
  , Item(ItemDef, ItemLet)
  , Lam(Lam)
  , Let(Let)
  , Type
  , Val(ValApp, ValCase, ValLam)
  )
import AstUtil (inPat)
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
