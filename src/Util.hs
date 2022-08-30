module Util where

import Data.Text (Text)
import qualified Data.Text as T

toText :: Show a => a -> Text
toText = T.pack . show