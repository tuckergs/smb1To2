
{-# LANGUAGE MonoLocalBinds #-}

module ReadCommon where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Data.Bits
import Data.Word
import System.IO

import FloatStuff
import Types
import Vector




readChar :: FileIO Char
readChar = ReaderT hGetChar

readChars :: FixedReplicateA n => FileIO (Vector n Char)
readChars = fixedReplicateA readChar

readByte :: Integral a => FileIO a
readByte = fromIntegral . fromEnum <$> readChar

readHalf :: (Bits a, Integral a) => FileIO a
readHalf = fmap (\(a:b:[]) -> shiftL a 8 .|. b) $ replicateM 2 readByte 

readWord :: (Bits a, Integral a) => FileIO a
readWord = fmap (\(a:b:[]) -> shiftL a 16 .|. b) $ replicateM 2 readHalf 

readFloat :: FileIO Float
readFloat = fmap hexToFloat readWord


data GlobalOffsets = GlobalOffsets {
  _goalGlobalOffset :: Offset ,
  _bumperGlobalOffset :: Offset ,
  _jamabarGlobalOffset :: Offset ,
  _bananaGlobalOffset :: Offset ,
  _levelModelGlobalOffset :: Offset ,
  _reflectiveModelGlobalOffset :: Offset
}
