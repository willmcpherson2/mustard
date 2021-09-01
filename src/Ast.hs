module Ast
  ( Ast(..)
  , Item(..)
  , Let(..)
  , Def(..)
  , Expr(..)
  , Ctor(..)
  , Type(..)
  , Val(..)
  , Fun(..)
  , Lit(..)
  , Lam(..)
  , Case(..)
  , AppVal(..)
  , Pat(..)
  , AppPat(..)
  , mkAst
  ) where

import Bexpr (Bexpr(..))
import Control.Monad ((<=<))
import Data.Foldable (asum)
import Error (Error(..), Fallible)
import Sexpr
  ( Atom(Lit, Name, Name, Unit)
  , Name(Lower, Upper)
  , Op(..)
  , Part(Anon)
  , Path(..)
  )
import qualified Sexpr (Lit(..))

newtype Ast = Ast [Fallible Item]
  deriving Show

data Item
  = ItemLet Let
  | ItemDef Def
  deriving Show

data Let = Let (Fallible Part) Expr
  deriving Show

data Def = Def (Fallible Part) [Fallible Ctor]
  deriving Show

data Expr = Expr (Fallible Type) (Fallible Val)
  deriving Show

data Ctor = Ctor (Fallible Part) [Fallible Type]
  deriving Show

data Type
  = TypeName (Fallible Path)
  | TypeUnit
  | TypeFun Fun
  | TypeInfer Int
  deriving Show

data Val
  = ValName (Fallible Path)
  | ValCtor (Fallible Path)
  | ValLit Lit
  | ValLam Lam
  | ValCase Case
  | ValApp AppVal
  deriving Show

data Fun = Fun (Fallible Type) (Fallible Type)
  deriving Show

data Lit
  = LitInt Int
  | LitFloat Float
  | LitUnit
  deriving Show

data Lam = Lam Part (Fallible Pat) Expr
  deriving Show

data Case = Case Expr [Fallible Lam]
  deriving Show

data AppVal = AppVal Expr Expr
  deriving Show

data Pat
  = PatBinder Part
  | PatCtor (Fallible Path)
  | PatLit Lit
  | PatApp AppPat
  deriving Show

data AppPat = AppPat (Fallible Pat) (Fallible Pat)
  deriving Show

--------------------------------------------------------------------------------

mkAst :: Bexpr -> Ast
mkAst bexpr = case bexpr of
  Branch App l r ->
    let Ast items = mkAst l in Ast $ items ++ [orLeft ExpectedItem mkItem r]
  bexpr -> Ast [orLeft ExpectedItem mkItem bexpr]

mkItem :: Bexpr -> Maybe Item
mkItem = first [Just . ItemLet <=< mkLet, Just . ItemDef <=< mkDef]

mkLet :: Bexpr -> Maybe Let
mkLet bexpr = do
  Branch Equal l r <- return bexpr
  Just $ Let (orLeft ExpectedLower mkLowerBinder l) (mkExpr r)

mkDef :: Bexpr -> Maybe Def
mkDef bexpr = do
  Branch Colon l r <- return bexpr
  Just $ Def (orLeft ExpectedUpper mkUpperBinder l) (mkCtors r)
  where
    mkCtors bexpr = case bexpr of
      Branch Pipe l r -> orLeft ExpectedCtor mkCtor l : mkCtors r
      bexpr -> [orLeft ExpectedCtor mkCtor bexpr]

mkExpr :: Bexpr -> Expr
mkExpr bexpr = case bexpr of
  Branch Colon l r ->
    Expr (orLeft ExpectedType mkType l) (orLeft ExpectedVal mkVal r)
  bexpr -> Expr (Right $ TypeInfer 0) (orLeft ExpectedVal mkVal bexpr)

mkCtor :: Bexpr -> Maybe Ctor
mkCtor bexpr = case bexpr of
  Leaf atom -> Just $ Ctor (orLeft ExpectedUpper mkUpperBinder (Leaf atom)) []
  Branch App l r -> case mkCtor l of
    Just (Ctor binder types) ->
      Just $ Ctor binder (types ++ [orLeft ExpectedType mkType r])
    _ -> Nothing
  _ -> Nothing

mkType :: Bexpr -> Maybe Type
mkType = first
  [ Just . TypeName . Right <=< mkUpper
  , Just . const TypeUnit <=< mkUnit
  , Just . TypeFun <=< mkFun
  ]

mkVal :: Bexpr -> Maybe Val
mkVal = first
  [ Just . ValName . Right <=< mkLower
  , Just . ValCtor . Right <=< mkUpper
  , Just . ValLit <=< mkLit
  , Just . ValLam <=< mkLam
  , Just . ValCase <=< mkCase
  , Just . ValApp <=< mkAppVal
  ]

mkFun :: Bexpr -> Maybe Fun
mkFun bexpr = do
  Branch Arrow l r <- return bexpr
  Just $ Fun (orLeft ExpectedType mkType l) (orLeft ExpectedType mkType r)

mkLit :: Bexpr -> Maybe Lit
mkLit = first
  [ Just . LitInt <=< mkInt
  , Just . LitFloat <=< mkFloat
  , Just . const LitUnit <=< mkUnit
  ]

mkLam :: Bexpr -> Maybe Lam
mkLam bexpr = do
  Branch Arrow l r <- return bexpr
  Just $ Lam (Anon 0) (orLeft ExpectedPat mkPat l) (mkExpr r)

mkCase :: Bexpr -> Maybe Case
mkCase bexpr = do
  Branch Question l r <- return bexpr
  Just $ Case (mkExpr l) (mkLams r)
  where
    mkLams bexpr = case bexpr of
      Branch App l r -> mkLams l ++ [orLeft ExpectedLam mkLam r]
      bexpr -> [orLeft ExpectedLam mkLam bexpr]

mkAppVal :: Bexpr -> Maybe AppVal
mkAppVal bexpr = do
  Branch App l r <- return bexpr
  Just $ AppVal (mkExpr l) (mkExpr r)

mkPat :: Bexpr -> Maybe Pat
mkPat = first
  [ Just . PatBinder <=< mkLowerBinder
  , Just . PatCtor . Right <=< mkUpper
  , Just . PatLit <=< mkLit
  , Just . PatApp <=< mkAppPat
  ]

mkAppPat :: Bexpr -> Maybe AppPat
mkAppPat bexpr = do
  Branch App l r <- return bexpr
  Just $ AppPat (orLeft ExpectedPat mkPat l) (orLeft ExpectedPat mkPat r)

--------------------------------------------------------------------------------

mkLower :: Bexpr -> Maybe Path
mkLower bexpr = case bexpr of
  Leaf (Name (Lower path)) -> Just path
  _ -> Nothing

mkUpper :: Bexpr -> Maybe Path
mkUpper bexpr = case bexpr of
  Leaf (Name (Upper path)) -> Just path
  _ -> Nothing

mkLowerBinder :: Bexpr -> Maybe Part
mkLowerBinder bexpr = case bexpr of
  Leaf (Name (Lower (Path [] name))) -> Just name
  _ -> Nothing

mkUpperBinder :: Bexpr -> Maybe Part
mkUpperBinder bexpr = case bexpr of
  Leaf (Name (Upper (Path [] name))) -> Just name
  _ -> Nothing

mkUnit :: Bexpr -> Maybe ()
mkUnit bexpr = do
  Leaf Unit <- return bexpr
  Just ()

mkFloat :: Bexpr -> Maybe Float
mkFloat bexpr = do
  Leaf (Lit (Sexpr.Float float)) <- return bexpr
  Just float

mkInt :: Bexpr -> Maybe Int
mkInt bexpr = do
  Leaf (Lit (Sexpr.Int int)) <- return bexpr
  Just int

--------------------------------------------------------------------------------

first :: [Bexpr -> Maybe a] -> Bexpr -> Maybe a
first parsers bexpr = asum $ map ($ bexpr) parsers

orLeft :: Error -> (Bexpr -> Maybe a) -> Bexpr -> Fallible a
orLeft error parser bexpr = maybe (Left error) Right (parser bexpr)
