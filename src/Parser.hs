module Parser (parseProgramFile) where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
    symbolChar,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Types

{- Megaparsec helpers -}

type Parser a = Parsec Void Text a

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parenthesized :: Parser a -> Parser a
parenthesized = between leftParenToken rightParenToken

{- Tokens -}

leftParenToken :: Parser Text
leftParenToken = lexeme (string "(")

rightParenToken :: Parser Text
rightParenToken = lexeme (string ")")

colonToken :: Parser Text
colonToken = lexeme (string ":")

equalToken :: Parser Text
equalToken = lexeme (string "=")

lambdaToken :: Parser Text
lambdaToken = lexeme (string "lambda")

arrowToken :: Parser Text
arrowToken = lexeme (string "->")

trueToken :: Parser Text
trueToken = lexeme (string "true")

falseToken :: Parser Text
falseToken = lexeme (string "false")

unitToken :: Parser Text
unitToken = lexeme (string "unit")

commaToken :: Parser Text
commaToken = lexeme (string ",")

defineToken :: Parser Text
defineToken = lexeme (string "define")

getToken :: Parser Text
getToken = lexeme (string "get")

ifToken :: Parser Text
ifToken = lexeme (string "if")

reservedIdentifiers :: [Text]
reservedIdentifiers = ["define", "lambda", "if", "false", "true", "unit", "get"]

symbols :: [Char]
symbols = ['+', '-', '*', '=']

identifierToken :: Parser Text
identifierToken =
  lexeme $ do
    firstChar <- letterChar <|> oneOf symbols
    rest <- many (alphaNumChar <|> oneOf symbols)
    let value = T.pack (firstChar : rest)
    if value `elem` reservedIdentifiers
      then fail ("Illegal keyword: " ++ show value)
      else pure value

integerToken :: Parser Int
integerToken = lexeme L.decimal

{- Type Parsers -}

tyIntParser :: Parser Ty
tyIntParser = do
  _ <- lexeme $ string "Int"
  pure TyInt

tyBoolParser :: Parser Ty
tyBoolParser = do
  _ <- lexeme $ string "Bool"
  pure TyBool

tyUnitParser :: Parser Ty
tyUnitParser = do
  _ <- lexeme $ string "Unit"
  pure TyUnit

tyProductParser :: Parser Ty
tyProductParser = parenthesized $ do
  types <- sepBy tyParser commaToken
  pure (TyProduct (V.fromList types))

tyFunctionParser :: Parser Ty
tyFunctionParser = parenthesized $ do
  input <- tyParser
  _ <- arrowToken
  output <- tyParser
  pure (TyFunction input output)

tyParser :: Parser Ty
tyParser =
  tyIntParser
    <|> tyBoolParser
    <|> try tyFunctionParser
    <|> tyProductParser

{- Term/Literal Parsers -}

varParser :: Parser Term
varParser = label "var" $ do
  name <- identifierToken
  pure (TmVar name)

absParser :: Parser Term
absParser = label "abs" $
  parenthesized $ do
    _ <- lambdaToken
    (paramNames, paramTys) <- unzip <$> paramListParser
    body <- termParser
    pure (TmAbs paramNames body paramTys)
  where
    paramListParser :: Parser [(Text, Ty)]
    paramListParser = parenthesized (paramParser `sepBy` commaToken)

    paramParser :: Parser (Text, Ty)
    paramParser = do
      name <- identifierToken
      _ <- colonToken
      ty <- tyParser
      pure (name, ty)

appParser :: Parser Term
appParser = label "app" $
  parenthesized $ do
    fn <- termParser
    args <- many termParser
    pure (TmApp fn args)

tupleParser :: Parser Term
tupleParser = label "tuple" $
  parenthesized $ do
    terms <- sepBy termParser commaToken
    pure (TmTuple (V.fromList terms))

getParser :: Parser Term
getParser = label "get" $
  parenthesized $ do
    _ <- getToken
    tuple <- termParser
    n <- integerToken
    pure (TmGet tuple n)

ifParser :: Parser Term
ifParser = label "if" $
  parenthesized $ do
    _ <- ifToken
    condition <- termParser
    consequent <- termParser
    alternate <- termParser
    pure (TmIf condition consequent alternate)

literalTermParser :: Parser Term
literalTermParser =
  label "literal" $ do
    literal <- intLiteralParser <|> boolLiteralParser
    pure (TmLit literal)
  where
    intLiteralParser :: Parser Literal
    intLiteralParser =
      label "num literal" $
        LiInt <$> integerToken

    boolLiteralParser :: Parser Literal
    boolLiteralParser =
      label "bool literal" $
        trueLiteralParser <|> falseLiteralParser
      where
        trueLiteralParser = trueToken $> LiBool True
        falseLiteralParser = falseToken $> LiBool False

    unitLiteralParser :: Parser Literal
    unitLiteralParser =
      label "unit literal" $
        unitToken $> LiUnit

termParser :: Parser Term
termParser =
  literalTermParser
    <|> varParser
    <|> try ifParser
    <|> try getParser
    <|> try absParser
    <|> try tupleParser
    <|> appParser

{- Statement Parsers -}

defineStatementParser :: Parser Statement
defineStatementParser = parenthesized $ do
  _ <- defineToken
  name <- identifierToken
  _ <- colonToken
  ty <- tyParser
  value <- termParser
  pure (StDefine name value ty)

expressionStatementParser :: Parser Statement
expressionStatementParser = do
  value <- termParser
  pure (StExpression value)

statementParser :: Parser Statement
statementParser = try defineStatementParser <|> expressionStatementParser

programParser :: Parser [Statement]
programParser = do
  skipSpace
  statements <- many statementParser
  eof
  pure statements

{- I/O -}

parseProgram :: Text -> FilePath -> Either String [Statement]
parseProgram input path =
  let parseOutput = parse programParser path input
   in case parseOutput of
        Left err -> Left (errorBundlePretty err)
        Right statements -> Right statements

parseProgramFile :: FilePath -> IO (Either String [Statement])
parseProgramFile path = do
  source <- TIO.readFile path
  pure (parseProgram source path)
