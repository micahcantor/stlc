module Eval (evalProgram) where

import qualified Context
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldlM)
import qualified Data.HashTable.IO as H
import qualified Data.Maybe as Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector ((!))
import Debug.Trace (trace, traceM, traceShowM)
import Primitive (primitiveValues)
import Types
import qualified Util

runEval :: EvalM a -> IO (Either RuntimeError a)
runEval = runExceptT . unEvalM

evalStatement :: EvalContext -> Statement -> EvalM (Maybe Value)
evalStatement ctx statement = case statement of
  StDefine name term _ -> do
    value <- evalTerm ctx term
    Context.bind ctx name value
    pure Nothing
  StExpression term -> do
    Just <$> evalTerm ctx term

evalTerm :: EvalContext -> Term -> EvalM Value
evalTerm ctx term = case term of
    TmVar name -> do
      value <- Context.lookup ctx name
      case value of
        Just value -> pure value
        Nothing -> throwError (UndefinedBinding name)
    TmAbs paramNames body _ ->
      evalAbs ctx paramNames body
    TmApp fn args -> do
      fn' <- evalTerm ctx fn
      case fn' of
        VaFunction f -> do
          argValues <- mapM (evalTerm ctx) args
          apply f argValues
        _ -> throwError DefaultError -- impossible unless typechecker is broken
    TmIf condition consequent alternate -> do
      condition' <- evalTerm ctx condition
      case condition' of
        VaBool b ->
          if b then evalTerm ctx consequent else evalTerm ctx alternate
        _ -> throwError DefaultError
    TmTuple terms -> do
      values <- mapM (evalTerm ctx) terms
      pure (VaTuple values)
    TmGet tuple index -> do
      tuple' <- evalTerm ctx tuple
      case tuple' of
        VaTuple values -> pure (values ! index)
        _ -> throwError DefaultError
    TmLit lit -> case lit of
      LiBool b -> pure (VaBool b)
      LiInt i -> pure (VaInt i)
      LiUnit -> pure VaUnit

evalAbs :: EvalContext -> [Text] -> Term -> EvalM Value
evalAbs ctx params body = case params of
  [] -> pure (VaFunction (const (evalTerm ctx body)))
  (name : names) -> pure $
    VaFunction $ \value -> do
      ctx' <- Context.copy ctx
      Context.bind ctx' name value
      case names of
        [] -> evalTerm ctx' body
        names' -> evalAbs ctx' names' body

apply :: (Value -> EvalM Value) -> [Value] -> EvalM Value
apply f args = do
  case args of
    [] -> pure VaUnit
    [arg] -> f arg
    (arg : args) -> do
      arg' <- f arg
      case arg' of
        VaFunction g -> apply g args
        _ -> pure arg'

evalProgram :: [Statement] -> IO (Either RuntimeError [Value])
evalProgram statements = do
  ctx <- Context.defaultEvalContext
  evalResult <- runEval (mapM (evalStatement ctx) statements)
  pure (Maybe.catMaybes <$> evalResult)
