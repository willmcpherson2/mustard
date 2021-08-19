module Ast (Ast(..), mkAst) where

import Bexpr (Bexpr(..))
import Control.Monad ((<=<))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Error (Error(..))
import Sexpr
  (Atom(Lit, Name, Unit), Name(Lower, NameError, Upper), Op(..), Path(..))
import qualified Sexpr (Lit(..))

newtype Ast = Ast [Item]
  deriving Show

data Item
  = ItemLet Let
  | ItemDef Def
  | ItemError Error
  deriving Show

data Let = Let Path Expr | LetError Error
  deriving Show

data Def = Def Path [Ctor] | DefError Error
  deriving Show

data Expr = Expr (Maybe Type) Val
  deriving Show

data Ctor = Ctor Path [Type] | CtorError Error
  deriving Show

data Type
  = TypeName Path
  | TypeUnit
  | TypeFun Fun
  | TypeError Error
  deriving Show

data Val
  = ValName Path
  | ValCtor Path
  | ValLit Lit
  | ValLam Lam
  | ValCase Case
  | ValApp AppVal
  | ValError Error
  deriving Show

data Fun = Fun Type Type
  deriving Show

data Lit
  = LitInt Int
  | LitFloat Float
  | LitUnit
  deriving Show

data Lam = Lam Path Expr | LamError Error
  deriving Show

data Case = Case Expr [Alt] | CaseError Error
  deriving Show

data AppVal = AppVal Expr Expr
  deriving Show

data Alt = Alt Pat Expr | AltError Error
  deriving Show

data Pat
  = PatBinder Path
  | PatCtor Path
  | PatLit Lit
  | PatApp AppPat
  | PatError Error
  deriving Show

data AppPat = AppPat Pat Pat
  deriving Show

--------------------------------------------------------------------------------

mkAst :: Bexpr -> Ast
mkAst bexpr = case bexpr of
  Branch App l r -> let Ast items = mkAst l in Ast $ items ++ [mkItem r]
  bexpr -> Ast [mkItem bexpr]

mkItem :: Bexpr -> Item
mkItem = firstOr
  (ItemError ExpectedItem)
  [Just . ItemLet <=< mkLet, Just . ItemDef <=< mkDef]

mkLet :: Bexpr -> Maybe Let
mkLet bexpr = do
  Branch Equal l r <- return bexpr
  Just $ Let (mkLowerError l) (mkExpr r)

mkDef :: Bexpr -> Maybe Def
mkDef bexpr = do
  Branch Colon l r <- return bexpr
  Just $ Def (mkUpperError l) (mkCtors r)
  where
    mkCtors bexpr = case bexpr of
      Branch Pipe l r -> mkCtor l : mkCtors r
      bexpr -> [mkCtor bexpr]

mkExpr :: Bexpr -> Expr
mkExpr bexpr = case bexpr of
  Branch Colon l r -> Expr (Just (mkType l)) (mkVal r)
  bexpr -> Expr Nothing (mkVal bexpr)

mkCtor :: Bexpr -> Ctor
mkCtor bexpr = case bexpr of
  Leaf name -> Ctor (mkUpperError (Leaf name)) []
  Branch App l r -> case mkCtor l of
    Ctor binder types -> Ctor binder (types ++ [mkType r])
    error -> error
  _ -> CtorError ExpectedCtor

mkType :: Bexpr -> Type
mkType = firstOr
  (TypeError ExpectedType)
  [ Just . TypeName <=< mkUpper
  , Just . const TypeUnit <=< mkUnit
  , Just . TypeFun <=< mkFun
  ]

mkVal :: Bexpr -> Val
mkVal = firstOr
  (ValError ExpectedVal)
  [ Just . ValName <=< mkLower
  , Just . ValCtor <=< mkUpper
  , Just . ValLit <=< mkLit
  , Just . ValLam <=< mkLam
  , Just . ValCase <=< mkCase
  , Just . ValApp <=< mkAppVal
  ]

mkFun :: Bexpr -> Maybe Fun
mkFun bexpr = do
  Branch Arrow l r <- return bexpr
  Just $ Fun (mkType l) (mkType r)

mkLit :: Bexpr -> Maybe Lit
mkLit = first
  [ Just . LitInt <=< mkInt
  , Just . LitFloat <=< mkFloat
  , Just . const LitUnit <=< mkUnit
  ]

mkLam :: Bexpr -> Maybe Lam
mkLam bexpr = do
  Branch Arrow l r <- return bexpr
  Just $ Lam (mkLowerError l) (mkExpr r)

mkCase :: Bexpr -> Maybe Case
mkCase bexpr = do
  Branch Question l r <- return bexpr
  Just $ Case (mkExpr l) (mkAlts r)
  where
    mkAlts bexpr = case bexpr of
      Branch App l r -> mkAlts l ++ [mkAlt r]
      bexpr -> [mkAlt bexpr]

mkAppVal :: Bexpr -> Maybe AppVal
mkAppVal bexpr = do
  Branch App l r <- return bexpr
  Just $ AppVal (mkExpr l) (mkExpr r)

mkAlt :: Bexpr -> Alt
mkAlt bexpr = case bexpr of
  Branch Arrow l r -> Alt (mkPat l) (mkExpr r)
  _ -> AltError ExpectedAlt

mkPat :: Bexpr -> Pat
mkPat = firstOr
  (PatError ExpectedPat)
  [ Just . PatBinder <=< mkLower
  , Just . PatCtor <=< mkUpper
  , Just . PatLit <=< mkLit
  , Just . PatApp <=< mkAppPat
  ]

mkAppPat :: Bexpr -> Maybe AppPat
mkAppPat bexpr = do
  Branch App l r <- return bexpr
  Just $ AppPat (mkPat l) (mkPat r)

--------------------------------------------------------------------------------

first :: [Bexpr -> Maybe a] -> Bexpr -> Maybe a
first parsers bexpr = asum $ map ($ bexpr) parsers

firstOr :: a -> [Bexpr -> Maybe a] -> Bexpr -> a
firstOr error parsers bexpr = fromMaybe error (first parsers bexpr)

mkLower :: Bexpr -> Maybe Path
mkLower bexpr = case bexpr of
  Leaf (Name (Lower path)) -> Just path
  Leaf (Name (NameError error)) -> Just (PathError error)
  _ -> Nothing

mkUpper :: Bexpr -> Maybe Path
mkUpper bexpr = case bexpr of
  Leaf (Name (Upper path)) -> Just path
  Leaf (Name (NameError error)) -> Just (PathError error)
  _ -> Nothing

mkLowerError :: Bexpr -> Path
mkLowerError bexpr = fromMaybe (PathError ExpectedLower) (mkLower bexpr)

mkUpperError :: Bexpr -> Path
mkUpperError bexpr = fromMaybe (PathError ExpectedUpper) (mkUpper bexpr)

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
