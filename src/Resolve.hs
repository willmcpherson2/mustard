module Resolve (Referent(..), resolve) where

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
  , Pat
  , Val(ValApp, ValCase, ValLam)
  )
import Control.Applicative ((<|>))
import Data.Either (rights)
import Data.List.Extra (firstJust)
import Error (Fallible)
import Qualify (patBinders)
import Sexpr (Part(Anon, Named), Path(Path))

data Referent
  = RefLet Let
  | RefDef Def
  | RefCtor Ctor
  | RefLam Lam
  | RefPat Pat

resolve :: Path -> Ast -> Maybe [Referent]
resolve path (Ast items) = do
  let
    (part, parts) = case path of
      Path [] name -> (name, [])
      Path (part : parts) name -> (part, parts ++ [name])
  referent <- resolveItems part items
  referents <- resolveAll parts referent
  Just $ referent : referents
  where
    resolveAll parts referent = case parts of
      [] -> Just []
      part : parts -> do
        referent <- resolveReferent part referent
        referents <- resolveAll parts referent
        Just $ referent : referents

resolveItems :: Part -> [Fallible Item] -> Maybe Referent
resolveItems part items = firstJust
  (getReferentMatch part)
  (map asReferent $ rights items)
  where
    asReferent item = case item of
      ItemLet le -> RefLet le
      ItemDef def -> RefDef def

resolveReferent :: Part -> Referent -> Maybe Referent
resolveReferent name referent = case referent of
  RefLet (Let _ (Expr _ val)) -> resolveVal name val
  RefDef (Def _ ctors) ->
    firstJust (getReferentMatch name) (map RefCtor $ rights ctors)
  RefLam (Lam _ pat (Expr _ val)) -> case name of
    Anon _ -> resolveVal name val
    Named _ -> case pat of
      Right pat -> if name `elem` patBinders (Right pat)
        then Just $ RefPat pat
        else Nothing
      Left _ -> Nothing
  _ -> Nothing

resolveVal :: Part -> Fallible Val -> Maybe Referent
resolveVal name (Right val) = case val of
  ValLam lam -> getReferentMatch name (RefLam lam)
  ValCase (Case _ lams) ->
    firstJust (getReferentMatch name) (map RefLam $ rights lams)
  ValApp (AppVal (Expr _ l) (Expr _ r)) ->
    resolveVal name l <|> resolveVal name r
  _ -> Nothing
resolveVal _ _ = Nothing

getReferentMatch :: Part -> Referent -> Maybe Referent
getReferentMatch name referent =
  if referentMatch name referent then Just referent else Nothing

referentMatch :: Part -> Referent -> Bool
referentMatch name referent = case referent of
  RefLet (Let (Right name') _) -> name' == name
  RefDef (Def (Right name') _) -> name' == name
  RefCtor (Ctor (Right name') _) -> name' == name
  RefLam (Lam id _ _) -> Anon id == name
  RefPat pat -> name `elem` patBinders (Right pat)
  _ -> False
