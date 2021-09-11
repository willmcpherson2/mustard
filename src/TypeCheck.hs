module TypeCheck (typeCheck) where

import Ast
  ( AppVal(AppVal)
  , Ast
  , Case(Case)
  , Expr(..)
  , Fun(Fun)
  , Lam(Lam)
  , Let(Let)
  , Lit(LitUnit)
  , Pat(PatBinder)
  , Type(TypeFun, TypeInfer, TypeUnit)
  , Val(ValApp, ValCase, ValCtor, ValLam, ValLit, ValName)
  )
import AstUtil (foldExprs, mapExprTypes)
import Data.Either (rights)
import Data.Foldable (asum)
import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Resolve
  (Ref(RefCase, RefLam, RefLet), binderType, ctorType, patType, resolve)
import Sexpr (namePart)

typeCheck :: Ast -> Ast
typeCheck = infer . initTypes

initTypes :: Ast -> Ast
initTypes = fst . foldExprs initType 0

initType :: Expr -> Int -> (Expr, Int)
initType expr n = case expr of
  Expr (Right (TypeInfer _)) val -> case val of
    Right (ValLam Lam{}) ->
      let
        arg = Right $ TypeInfer n
        ret = Right $ TypeInfer $ n + 1
        fun = TypeFun $ Fun arg ret
      in (Expr (Right fun) val, n + 2)
    _ -> (Expr (Right $ TypeInfer n) val, n + 1)
  _ -> (expr, n)

--------------------------------------------------------------------------------

data Constraint = Constraint Type Type
  deriving (Eq, Show)

find :: Type -> [Constraint] -> Maybe Type
find ty = firstJust (\(Constraint l r) -> if l == ty then Just r else Nothing)

--------------------------------------------------------------------------------

infer :: Ast -> Ast
infer ast =
  let
    constraints = constrain ast
    unified = unify [] constraints
    transitted = transit [] unified
    ast' = substitute transitted ast
  in ast'

constrain :: Ast -> [Constraint]
constrain ast = snd $ foldExprs (go ast) [] ast
  where go ast expr set = (expr, constrainExpr ast expr ++ set)

constrainExpr :: Ast -> Expr -> [Constraint]
constrainExpr ast (Expr (Right ty) (Right val)) = case val of
  ValLit lit -> case lit of
    LitUnit -> [Constraint ty TypeUnit]
    _ -> undefined
  ValCtor (Right path) -> maybeToList $ Constraint ty <$> ctorType ast path
  ValName (Right path) -> case resolve ast path of
    RefLet (Let _ (Expr letTy _)) -> case letTy of
      Left _ -> []
      Right letTy -> [Constraint ty letTy]
    RefLam lamTy (Lam _ pat _) -> case pat of
      Right (PatBinder _) -> case lamTy of
        Right (TypeFun (Fun (Right l) _)) -> [Constraint ty l]
        _ -> []
      Right pat ->
        maybeToList $ Constraint ty <$> binderType ast (namePart path) pat
      _ -> []
    RefCase _ (Case (Expr caseExprTy _) _) (Lam _ pat _) -> case pat of
      Right (PatBinder _) -> case caseExprTy of
        Right caseExprTy -> [Constraint ty caseExprTy]
        _ -> []
      Right pat ->
        maybeToList $ Constraint ty <$> binderType ast (namePart path) pat
      _ -> []
    _ -> undefined
  ValApp (AppVal (Expr (Right l) _) (Expr (Right r) _)) ->
    [Constraint l (TypeFun $ Fun (Right r) (Right ty))]
  ValLam (Lam _ (Right pat) (Expr (Right lamExprTy) _)) -> case ty of
    TypeFun (Fun (Right l) (Right r)) ->
      Constraint r lamExprTy : maybeToList (Constraint l <$> patType ast pat)
    _ -> []
  ValCase (Case (Expr (Right exprTy) _) lams) ->
    mapMaybe (constrainToLamPat ast exprTy) (rights lams)
      ++ mapMaybe (constrainToLamTy ty) (rights lams)
    where
      constrainToLamTy ty (Lam _ _ (Expr lamTy _)) = case lamTy of
        Right lamTy -> Just $ Constraint ty lamTy
        _ -> Nothing
      constrainToLamPat ast ty (Lam _ pat _) = case pat of
        Right pat -> Constraint ty <$> patType ast pat
        _ -> Nothing
  _ -> []
constrainExpr _ _ = []

--------------------------------------------------------------------------------

unify :: [Constraint] -> [Constraint] -> [Constraint]
unify context = foldr (go context) []
  where
    go context constraint results =
      results ++ unifyConstraint (context ++ results) constraint

unifyConstraint :: [Constraint] -> Constraint -> [Constraint]
unifyConstraint context constraint = fromMaybe [constraint] $ asum $ map
  (($ constraint) . ($ context))
  [rightVar, functions, redundant, duplicate, leftDuplicate]

rightVar :: [Constraint] -> Constraint -> Maybe [Constraint]
rightVar context (Constraint l r) = do
  TypeInfer _ <- Just r
  case l of
    TypeInfer _ -> Nothing
    _ -> Just $ unify context [Constraint r l]

functions :: [Constraint] -> Constraint -> Maybe [Constraint]
functions context (Constraint l r) = do
  TypeFun (Fun (Right lArg) (Right lRet)) <- Just l
  TypeFun (Fun (Right rArg) (Right rRet)) <- Just r
  Just $ unify context [Constraint lArg rArg, Constraint lRet rRet]

redundant :: [Constraint] -> Constraint -> Maybe [Constraint]
redundant _ (Constraint l r) = if l == r then Just [] else Nothing

duplicate :: [Constraint] -> Constraint -> Maybe [Constraint]
duplicate context constraint =
  if constraint `elem` context then Just [] else Nothing

leftDuplicate :: [Constraint] -> Constraint -> Maybe [Constraint]
leftDuplicate context (Constraint l r) = do
  r' <- find l context
  Just $ unify context [Constraint r' r]

--------------------------------------------------------------------------------

transit :: [Constraint] -> [Constraint] -> [Constraint]
transit results constraints = case constraints of
  [] -> results
  Constraint l r : constraints ->
    let
      result = Constraint l (transitType (results ++ constraints) r)
      results' = result : results
    in transit results' constraints

transitType :: [Constraint] -> Type -> Type
transitType context ty = case ty of
  TypeInfer _ -> case find ty context of
    Just r -> if r == ty then ty else transitType context r
    _ -> ty
  TypeFun (Fun (Right l) (Right r)) -> TypeFun
    $ Fun (Right $ transitType context l) (Right $ transitType context r)
  _ -> ty

--------------------------------------------------------------------------------

substitute :: [Constraint] -> Ast -> Ast
substitute set = mapExprTypes (fmap (\ty -> fromMaybe ty (find ty set)))
