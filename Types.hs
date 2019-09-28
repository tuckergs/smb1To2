
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, ScopedTypeVariables, TemplateHaskell #-}

module Types where

import Control.Lens
import Control.Monad.Reader
import Data.Word
import System.Exit
import System.IO

import Nat
import Vector

type Offset = Integer
data ListOffset = ListOffset Integer Integer
  deriving Show


data Dummy = Dummy
type FileIO = ReaderT Handle IO
type PlaceIO = ReaderT Offset FileIO

absSeek :: Offset -> FileIO ()
absSeek off = ReaderT $ \handle -> hSeek handle AbsoluteSeek off

tell :: FileIO Integer
tell = ReaderT hTell

seekAndDo :: Offset -> FileIO a -> PlaceIO a
seekAndDo off m = do
  ReaderT $ \mrk -> absSeek (mrk+off)
  lift m

syncThenLift :: FileIO a -> PlaceIO a
syncThenLift = seekAndDo 0

syncHandle = syncThenLift $ return ()


class MyLength a where
  myLength :: a -> Integer

instance MyLength (Vector Zero Char) where
  myLength _ = 0

instance forall (n::Nat) . MyLength (Vector n Char) => MyLength (Vector (Succ n) Char) where
  myLength _ = 1 + myLength (undefined :: Vector n Char)


type PosRot = Vector Nat20 Char
type PosRotScale = Vector Nat32 Char


data GoalType = BlueG | GreenG | RedG
  deriving Show



type StartPos = PosRot
type Bumper = PosRotScale
type Jamabar = PosRotScale
type Banana = Vector Nat16 Char
data Goal = Goal (Vector Nat18 Char) GoalType
  deriving Show
type LevelModel = String
type ReflectiveModel = String
type Triangle = Vector Nat64 Char
type CollisionParameters = Vector Nat24 Char

data AnimFrame = AnimFrame {
  _easing :: Vector Nat4 Char ,
  _time :: Float ,
  _value :: Float
} deriving Show
makeLenses ''AnimFrame

data AnimData = AnimData {
  _rotXFrames :: [AnimFrame] ,
  _rotYFrames :: [AnimFrame] ,
  _rotZFrames :: [AnimFrame] ,
  _posXFrames :: [AnimFrame] ,
  _posYFrames :: [AnimFrame] ,
  _posZFrames :: [AnimFrame]
}
makeLenses ''AnimData

data CollisionHeader = CollisionHeader {
  _rotCenter :: (Float,Float,Float) ,
  _initRot :: (Word16,Word16,Word16) ,
  _animType :: Word16 ,
  _animData :: AnimData ,
  _animLoopTime :: Float ,
  -- Apparently redundant model reference stuff?
  _triangles :: [Triangle] ,
  _triangleIndexLists :: [[Word16]] ,
  _collisionParameters :: CollisionParameters ,
  _goalListOffset :: ListOffset ,
  _bumperListOffset :: ListOffset ,
  _jamabarListOffset :: ListOffset ,
  _bananaListOffset :: ListOffset ,
  _levelModelListOffset :: ListOffset ,
  _reflectiveModelListOffset :: ListOffset
}
makeLenses ''CollisionHeader

data LZData = LZData {
  _collisionHeaders :: [CollisionHeader] ,
  _startPos :: StartPos ,
  _falloutY :: Vector Nat4 Char ,
  _goals :: [Goal] ,
  _bumpers :: [Bumper] ,
  _jamabars :: [Jamabar] ,
  _bananas :: [Banana] ,
  _levelModels :: [LevelModel] ,
  _reflectiveModels :: [ReflectiveModel] 
}
makeLenses ''LZData

  

