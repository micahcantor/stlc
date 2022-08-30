module Main where

import Control.Monad (forM_)
import Eval (evalProgram)
import Parser (parseProgramFile)
import System.Environment (getArgs)
import Typecheck (typecheckProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      parseResult <- parseProgramFile file
      case parseResult of
        Left parseError -> putStr parseError
        Right statements -> do
          typecheckResult <- typecheckProgram statements
          case typecheckResult of
            Left typeError -> print typeError
            Right _ -> do
              evalResult <- evalProgram statements
              case evalResult of
                Left runtimeError -> print runtimeError
                Right values -> forM_ values print
    _ -> putStrLn ("Expected filename, got " <> show args)
