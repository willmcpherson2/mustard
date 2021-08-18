module Error (Error(..), Fallible) where

data Error
  = UnexpectedOp
  | ExpectedItem
  | ExpectedName
  | ExpectedVal
  | ExpectedType
  | ExpectedCtor
  | ExpectedAlt
  | ExpectedPat
  deriving Show

type Fallible a = Either Error a
