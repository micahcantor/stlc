module Typecheck where

import qualified Context
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector ((!?))
import qualified Data.Vector as V
import Primitive (primitiveValueTypes)
import Types
import qualified Util
import Debug.Trace

runTypecheckM :: TypecheckM a -> IO (Either TypeError a)
runTypecheckM = runExceptT . unTypecheckM

typecheckStatement :: TypecheckContext -> Statement -> TypecheckM (Maybe Ty)
typecheckStatement ctx stmt = case stmt of
  StDefine name term ty -> do
    Context.bind ctx name ty
    termTy <- typecheck ctx term
    if termTy == ty
      then pure Nothing
      else throwError (MismatchError ty termTy ("In '" <> name <> "' definition: "))
    pure Nothing
  StExpression term ->
    Just <$> typecheck ctx term

typecheck :: TypecheckContext -> Term -> TypecheckM Ty
typecheck ctx term = case term of
  TmVar name -> do
    value <- Context.lookup ctx name
    case value of
      Just ty -> pure ty
      Nothing -> throwError (UndefinedVariableError name)
  TmAbs paramNames body paramTypes -> 
    typecheckAbs ctx body (zip paramNames paramTypes)
  TmApp fn args -> do
    fnType <- typecheck ctx fn
    argTypes <- mapM (typecheck ctx) args
    typecheckApplication fnType argTypes
  TmIf condition consequent alternate -> do
    conditionType <- typecheck ctx condition
    if conditionType == TyBool
      then do
        consequentType <- typecheck ctx consequent
        alternateType <- typecheck ctx alternate
        if consequentType == alternateType
          then pure consequentType
          else throwError (MismatchError consequentType alternateType "In 'if' branch: ")
      else throwError (MismatchError TyBool conditionType "In 'if' condition: ")
  TmTuple values -> do
    valueTypes <- mapM (typecheck ctx) values
    pure (TyProduct valueTypes)
  TmGet tuple index -> do
    tupleType <- typecheck ctx tuple
    case tupleType of
      TyProduct types -> do
        case types !? index of
          Just ty -> pure ty
          Nothing -> throwError (TupleRangeError (V.length types) index)
      _ -> 
        -- This is not quite right, I'm not sure how to recover the types 
        -- within the tuple in case of an error
        throwError (MismatchError (TyProduct V.empty) tupleType "In 'get' tuple: ")
  TmLit lit -> case lit of
    LiInt n -> pure TyInt
    LiBool b -> pure TyBool
    LiUnit -> pure TyUnit

typecheckAbs :: TypecheckContext -> Term -> [(Text, Ty)] -> TypecheckM Ty
typecheckAbs ctx body params =
  case params of
    [] -> do
      bodyType <- typecheck ctx body
      pure (TyFunction TyUnit bodyType)
    ((name, ty) : params') -> do
      ctx' <- Context.copy ctx
      Context.bind ctx' name ty
      outputTy <- case params' of
        [] -> typecheck ctx' body
        _ -> typecheckAbs ctx' body params'
      pure (TyFunction ty outputTy)

typecheckApplication :: Ty -> [Ty] -> TypecheckM Ty
typecheckApplication fnType argTypes =
  case fnType of
    TyFunction inputType outputType ->
      case argTypes of
        [] -> 
          if inputType == TyUnit
            then pure outputType
            else throwError (MismatchError TyUnit outputType "In application: ")
        (ty : tys) ->
          if inputType == ty
            then case tys of
              [] -> pure outputType
              _ -> typecheckApplication outputType tys
            else throwError (MismatchError outputType ty "In application: ")
    _ -> 
      -- Not sure how to recover/report the expected input and ouput types in this case
      throwError (MismatchError (TyFunction TyUnit TyUnit) fnType "Application not a function: ")

typecheckProgram :: [Statement] -> IO (Either TypeError [Ty])
typecheckProgram statements = do
  ctx <- Context.defaultTypecheckContext
  typecheckResult <- runTypecheckM (mapM (typecheckStatement ctx) statements)
  pure (Maybe.catMaybes <$> typecheckResult)