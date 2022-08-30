module Types where

import Control.Monad.Except
import qualified Data.HashTable.IO as H
import qualified Data.List as List (intercalate)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Util

type EvalContext = H.CuckooHashTable Text Value

data RuntimeError
  = UndefinedBinding Text
  | DefaultError
  deriving (Show)

newtype EvalM a = EvalM {unEvalM :: ExceptT RuntimeError IO a}
  deriving (Functor, Applicative, Monad, MonadError RuntimeError, MonadIO)

type TypecheckContext = H.CuckooHashTable Text Ty

data TypeError
  = MismatchError
      { expected :: Ty,
        actual :: Ty,
        message :: Text
      }
  | UndefinedVariableError Text
  | TupleRangeError Int Int

typeErrorMessage :: TypeError -> Text
typeErrorMessage typeError =
  case typeError of
    MismatchError expected actual message ->
      "Type Error: "
        <> message
        <> "Expected "
        <> Util.toText expected
        <> " but found "
        <> Util.toText actual
        <> "."
    UndefinedVariableError name -> "Undefined variable: '" <> name <> "'."
    TupleRangeError top actual ->
      "Expected tuple access in range 0 to "
        <> Util.toText (top - 1)
        <> " but found "
        <> Util.toText actual
        <> "."

instance Show TypeError where
  show typeError = show (typeErrorMessage typeError)

newtype TypecheckM a = TypeCheckM {unTypecheckM :: ExceptT TypeError IO a}
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadIO)

data Ty
  = TyInt
  | TyBool
  | TyUnit
  | TyProduct (Vector Ty)
  | TyFunction Ty Ty
  deriving (Show, Eq)

data Literal
  = LiInt Int
  | LiBool Bool
  | LiUnit
  deriving (Show, Eq)

data Term
  = TmVar Text
  | TmAbs [Text] Term [Ty]
  | TmApp Term [Term]
  | TmIf Term Term Term
  | TmTuple (Vector Term)
  | TmGet Term Int
  | TmLit Literal
  deriving (Show, Eq)

data Statement
  = StDefine Text Term Ty
  | StExpression Term
  deriving (Show, Eq)

data Value
  = VaUnit
  | VaInt Int
  | VaBool Bool
  | VaTuple (Vector Value)
  | VaFunction (Value -> EvalM Value)

instance Eq Value where
  a == b = case (a, b) of
    (VaUnit, VaUnit) -> True
    (VaInt n, VaInt k) -> n == k
    (VaBool b1, VaBool b2) -> b1 == b2
    (VaTuple xs, VaTuple ys) -> xs == ys
    _ -> False

instance Show Value where
  show v = case v of
    VaInt n -> show n
    VaBool b -> show b
    VaUnit -> "unit"
    VaTuple values ->
      "(" <> List.intercalate "," (V.toList (fmap show values)) <> ")"
    VaFunction f -> "<fn>"