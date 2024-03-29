module HexStuff where

import Numeric
import Text.Read
import Data.Char


newtype Hex a = Hex { unHex :: a }
  deriving Eq

instance Integral a => Read (Hex a) where
  readPrec = readS_to_Prec (\_ -> (\inp -> map (\(x,res) -> (Hex x, res)) (readHex inp)))

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex n) = showInHex n

showInHex :: (Integral a, Show a) => a -> String
showInHex = map toUpper . flip showHex ""
