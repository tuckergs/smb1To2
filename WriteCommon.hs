
{-# LANGUAGE LambdaCase, MonoLocalBinds, TemplateHaskell #-}

module WriteCommon where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Bits
import System.IO

import FloatStuff
import Types
import Vector

writeChar :: Char -> FileIO ()
writeChar x = ReaderT $ flip hPutChar x

writeChars :: FixedReplicateA n => Vector n Char -> FileIO ()
writeChars = mapM_ writeChar

writeByte :: (Bits a, Integral a) => a -> FileIO ()
writeByte x = ReaderT $ \handle -> hPutChar handle $ toEnum $ (.&. 0xff) $ fromIntegral x

writeHalf :: (Bits a, Integral a) => a -> FileIO ()
writeHalf x = mapM_ writeByte [x `shiftR` 8,x]

writeWord :: (Bits a, Integral a) => a -> FileIO ()
writeWord x = mapM_ writeHalf [x `shiftR` 16, x]

writeFloat :: Float -> FileIO ()
writeFloat = writeWord . floatToHex

type Offset6 = (Offset,Offset,Offset,Offset,Offset,Offset)

data WriteOffsets = WriteOffsets {
  _collisionTriangleOffsets :: [Integer] ,
  _indexPointerListOffsets :: [Integer] ,
  _indexListOffsets :: [[Integer]] ,
  _goalOffset :: Integer ,
  _bumperOffset :: Integer ,
  _jamabarOffset :: Integer ,
  _bananaOffset :: Integer ,
  _coneOffset :: Integer ,
  _sphereOffset :: Integer ,
  _cylinderOffset :: Integer ,
  _levelAOffset :: Integer ,
  _levelBOffset :: Integer ,
  _levelModelOffset :: Integer ,
  _levelNameOffsets :: [Integer] ,
  _reflectiveModelOffset :: Integer ,
  _reflectiveNameOffsets :: [Integer] ,
  _animHeaderOffset :: Integer ,
  _animKeyframeOffsets :: [Offset6]
} deriving Show
makeLenses ''WriteOffsets
