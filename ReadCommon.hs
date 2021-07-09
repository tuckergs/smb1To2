
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
import ReadMonad
import Types
import Vector

readChar :: CharIO Char
readChar = getChr

readChars :: FixedReplicateA n => CharIO (Vector n Char)
readChars = fixedReplicateA readChar

readByte :: Integral a => CharIO a
readByte = fromIntegral . fromEnum <$> readChar

readHalf :: (Bits a, Integral a) => CharIO a
readHalf = fmap (\(a:b:[]) -> shiftL a 8 .|. b) $ replicateM 2 readByte 

readWord :: (Bits a, Integral a) => CharIO a
readWord = fmap (\(a:b:[]) -> shiftL a 16 .|. b) $ replicateM 2 readHalf 

readFloat :: CharIO Float
readFloat = fmap hexToFloat readWord


data GlobalOffsets = GlobalOffsets {
  _goalGlobalOffset :: Offset ,
  _bumperGlobalOffset :: Offset ,
  _jamabarGlobalOffset :: Offset ,
  _bananaGlobalOffset :: Offset ,
  _coneGlobalOffset :: Offset ,
  _sphereGlobalOffset :: Offset ,
  _cylinderGlobalOffset :: Offset ,
  _levelModelGlobalOffset :: Offset ,
  _reflectiveModelGlobalOffset :: Offset
}
