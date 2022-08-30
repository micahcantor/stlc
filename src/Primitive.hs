module Primitive where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Types

primitiveValues :: [(Text, Value)]
primitiveValues =
  [ ("=", equalityPrimitive),
    ("+", plusPrimitive),
    ("-", minusPrimitive),
    ("*", multiplyPrimitive)
  ]

primitiveValueTypes :: [(Text, Ty)]
primitiveValueTypes =
  [ ("=", equalityPrimitiveType),
    ("+", numericPrimitiveType),
    ("-", numericPrimitiveType),
    ("*", numericPrimitiveType)
  ]

equalityPrimitive :: Value
equalityPrimitive = VaFunction $ \a -> pure $
  VaFunction $ \b -> pure (VaBool (a == b))

equalityPrimitiveType :: Ty
equalityPrimitiveType = TyFunction TyInt (TyFunction TyInt TyBool)

numericPrimitive :: (Int -> Int -> Int) -> Value
numericPrimitive op =
  VaFunction $ \(VaInt n) -> pure $
    VaFunction $ \(VaInt k) ->
      pure $ VaInt (n `op` k)

numericPrimitiveType :: Ty
numericPrimitiveType = TyFunction TyInt (TyFunction TyInt TyInt)

plusPrimitive :: Value
plusPrimitive = numericPrimitive (+)

minusPrimitive :: Value
minusPrimitive = numericPrimitive (-)

multiplyPrimitive :: Value
multiplyPrimitive = numericPrimitive (*)