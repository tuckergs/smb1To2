
{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables #-}

module ReadSMB1 (readSMB1) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Word
import System.Exit
import System.IO

import FloatStuff
import HexStuff
import Nat
import ReadCommon
import Types
import Vector

newtype LevelModelEntry = LevelModelEntry { getLevelModelEntry :: String }
newtype ReflectiveModelEntry = ReflectiveModelEntry { getReflectiveModelEntry :: String }

instance MyLength CollisionHeader where
  myLength _ = 196

instance MyLength Goal where
  myLength _ = 20

instance MyLength LevelModelEntry where
  myLength _ = 12

instance MyLength ReflectiveModelEntry where
  myLength _ = 8

instance MyLength AnimFrame where
  myLength _ = 20

bytesToGoalType :: Vector Nat2 Char -> IO GoalType
bytesToGoalType (Cons '\x42' (Cons '\x00' Nil)) = return BlueG
bytesToGoalType (Cons '\x47' (Cons '\x00' Nil)) = return GreenG
bytesToGoalType (Cons '\x52' (Cons '\x00' Nil)) = return RedG
bytesToGoalType _ = die $ "Invalid goal type detected. Check ur goalz!"


getGlobalOffsets :: FileIO GlobalOffsets
getGlobalOffsets = flip runReaderT 0x0 $
  let
    getOffsetAt r = seekAndDo r readWord
  in pure GlobalOffsets
    <*> getOffsetAt 0x1c
    <*> getOffsetAt 0x2c
    <*> getOffsetAt 0x34
    <*> getOffsetAt 0x3c
    <*> getOffsetAt 0x44
    <*> getOffsetAt 0x4c
    <*> getOffsetAt 0x54
    <*> getOffsetAt 0x5c
    <*> getOffsetAt 0x84

chasePointer :: Offset -> PlaceIO a -> PlaceIO a
chasePointer roff m = do
  off <- seekAndDo roff readWord
  -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex off)
  lift $ runReaderT (syncHandle >> m) off

chasePointerNullable :: Offset -> PlaceIO a -> PlaceIO (Maybe a)
chasePointerNullable roff m = do
  seekAndDo roff readWord >>= \case
    0x0 -> do
      -- liftIO $ hPutStrLn stderr $ "Encountered null, not jumping"
      return Nothing
    off -> do
      -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex off)
      lift $ fmap Just $ runReaderT (syncHandle >> m) off

chaseListPointer :: forall a . MyLength a => Offset -> PlaceIO a -> PlaceIO [a]
chaseListPointer loff m = do
  num <- seekAndDo loff readWord
  noff <- lift readWord
  -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex noff)
  forM (take num $ [0,myLength (undefined :: a)..]) $
    \off2 -> lift $ runReaderT (syncHandle >> m) (noff+off2) 

chaseListSimple :: Int -> Offset -> Integer -> PlaceIO a -> PlaceIO [a]
chaseListSimple num loff len m = do
  noff <- seekAndDo loff readWord
  -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex noff)
  forM (take num $ [0,len..]) $
    \off2 -> lift $ runReaderT (syncHandle >> m) (noff+off2) 

chaseListGivenLength :: Offset -> Integer -> PlaceIO a -> PlaceIO [a]
chaseListGivenLength loff len m = do
  num <- seekAndDo loff readWord
  noff <- lift readWord
  -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex noff)
  forM (take num $ [0,len..]) $
    \off2 -> lift $ runReaderT (syncHandle >> m) (noff+off2) 
  

readGoal :: FileIO Goal
readGoal = pure Goal 
  <*> readChars 
  <*> (readChars >>= lift . bytesToGoalType)

readString :: FileIO String
readString = readChar >>= \case
  '\0' -> return []
  c -> fmap (c:) readString

readListOffset :: Offset -> FileIO ListOffset
readListOffset initOffset = pure ListOffset
  <*> readWord
  <*> fmap (flip (-) initOffset) readWord

chaseAndReadTriIndexList :: FileIO [Word16]
chaseAndReadTriIndexList = do
  readWord >>= \case
    0x0 -> return []
    noff -> do
      runReaderT syncHandle noff
      fix $ \loop -> readHalf >>= \case
        0xffff -> return []
        ind -> fmap (ind:) loop

readCollisionHeader :: GlobalOffsets -> PlaceIO CollisionHeader
readCollisionHeader (GlobalOffsets off1 off2 off3 off4 off5 off6 off7 off8 off9) = do
  -- Calculate triangle stuff
  numTriLists <- pure (*) <*> seekAndDo 0x34 readWord <*> lift readWord
  triIndexLists <- chaseListSimple numTriLists 0x20 0x4 $ syncThenLift chaseAndReadTriIndexList
  let numTris = fromIntegral $ maximum $ (0:) $ map (+1) $ concat triIndexLists -- map (+1) is where it is to handle null case
  tris <- chaseListSimple numTris 0x1c (myLength (undefined :: Triangle)) $ syncThenLift readChars

  pure CollisionHeader
    <*> seekAndDo 0x0 (liftM3 (,,) readFloat readFloat readFloat)
    <*> seekAndDo 0xc (liftM3 (,,) readHalf readHalf readHalf)
    <*> seekAndDo 0x12 readHalf
    <*> fmap (maybe (AnimData [] [] [] [] [] []) id) (chasePointerNullable 0x14 readAnimHeader)
    <*> fmap (maybe 0 id) (chasePointerNullable 0x14 getAnimLoopTime)
    <*> pure tris
    <*> pure triIndexLists
    <*> seekAndDo 0x24 readChars
    <*> seekAndDo 0x3c (readListOffset off1) -- Goals
    <*> seekAndDo 0x4c (readListOffset off2) -- Bumpers
    <*> seekAndDo 0x54 (readListOffset off3) -- Jamabars
    <*> seekAndDo 0x5c (readListOffset off4) -- Bananas
    <*> seekAndDo 0x64 (readListOffset off5) -- Cones
    <*> seekAndDo 0x6c (readListOffset off6) -- Spheres
    <*> seekAndDo 0x74 (readListOffset off7) -- Cylinders
    <*> seekAndDo 0x7c (readListOffset off8) -- Level Models
    <*> seekAndDo 0x8c (readListOffset off9) -- Reflective Models. docs seem to imply it should be 0xa4, but that's wrong

readAnimFrame :: FileIO AnimFrame
readAnimFrame = liftM4 AnimFrame readChars readFloat readFloat readChars

traverseAnimHeader :: (a -> a -> a -> a -> a -> a -> b) -> (Offset -> PlaceIO a) -> PlaceIO b
traverseAnimHeader f k = 
  pure f
    <*> k 0
    <*> k 8
    <*> k 16
    <*> k 24
    <*> k 32
    <*> k 40

readAnimHeader = traverseAnimHeader AnimData $ flip chaseListPointer (syncThenLift readAnimFrame)
getAnimLoopTime = traverseAnimHeader mex act 
  where
    act loff = chaseListGivenLength loff 0x14 $ fmap hexToFloat $ seekAndDo 0x4 readWord
    mex l1 l2 l3 l4 l5 l6 = maximum $ (0:) $ l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6
  
readSMB1 :: FileIO LZData
readSMB1 = 
  getGlobalOffsets >>= \globalOffsets -> flip runReaderT 0x0 $ 
    pure LZData
    <*> chaseListPointer 0x8 (readCollisionHeader globalOffsets)
    <*> chasePointer 0x10 (syncThenLift readChars)
    <*> chasePointer 0x14 (syncThenLift readChars)
    <*> chaseListPointer 0x18 (syncThenLift readGoal)
    <*> chaseListPointer 0x28 (syncThenLift readChars) -- bumpears
    <*> chaseListPointer 0x30 (syncThenLift readChars) -- jamabears
    <*> chaseListPointer 0x38 (syncThenLift readChars) -- BA NA NA
    <*> chaseListPointer 0x40 (syncThenLift readChars) -- Cones
    <*> chaseListPointer 0x48 (syncThenLift readChars) -- Spheres
    <*> chaseListPointer 0x50 (syncThenLift readChars) -- Cylinders
    <*> fmap (map getLevelModelEntry)       (chaseListPointer 0x58 $ fmap LevelModelEntry $ chasePointer 0x4 (syncThenLift readString))
    <*> fmap (map getReflectiveModelEntry)  (chaseListPointer 0x80 $ fmap ReflectiveModelEntry $ chasePointer 0x0 (syncThenLift readString))
