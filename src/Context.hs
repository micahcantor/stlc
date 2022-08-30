module Context where

import Control.Monad.IO.Class (MonadIO (..))
import Data.HashTable.Class (HashTable)
import Data.HashTable.IO (IOHashTable)
import qualified Data.HashTable.IO as H
import Data.Text (Text)
import Primitive (primitiveValueTypes, primitiveValues)
import Types

{- This file contains lifted operations on IOHashTable for convenience
   in use with Eval and Typecheck -}

defaultEvalContext :: IO EvalContext
defaultEvalContext = H.fromList primitiveValues

defaultTypecheckContext :: IO TypecheckContext
defaultTypecheckContext = H.fromList primitiveValueTypes

bind :: (HashTable h, MonadIO m) => IOHashTable h Text v -> Text -> v -> m ()
bind env name value = liftIO (H.insert env name value)

lookup :: (HashTable h, MonadIO m) => IOHashTable h Text v -> Text -> m (Maybe v)
lookup env name = liftIO (H.lookup env name)

-- this is silly but couldn't find a better way
copy :: (HashTable h, MonadIO m) => IOHashTable h Text v -> m (IOHashTable h Text v)
copy env = liftIO (H.toList env >>= H.fromList)