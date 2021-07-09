
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
import ReadMonad
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

bytesToGoalType :: Vector Nat2 Char -> GoalType
bytesToGoalType (Cons '\x42' (Cons '\x00' Nil)) = BlueG
bytesToGoalType (Cons '\x47' (Cons '\x00' Nil)) = GreenG
bytesToGoalType (Cons '\x52' (Cons '\x00' Nil)) = RedG
bytesToGoalType _ = UnknownG


getGlobalOffsets :: ReadIO GlobalOffsets
getGlobalOffsets = absSeekThenDo 0 $
  let
    getOffsetAt r = seekThenChars r readWord
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

chasePointer :: Offset -> ReadIO a -> ReadIO a
chasePointer roff m = do
  off <- seekThenChars roff readWord
  -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex off)
  absSeekThenDo off m

chasePointerNullable :: Offset -> ReadIO a -> ReadIO (Maybe a)
chasePointerNullable roff m = do
  seekThenChars roff readWord >>= \case
    0x0 -> do
      -- liftIO $ hPutStrLn stderr $ "Encountered null, not jumping"
      return Nothing
    off -> do
      -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex off)
      Just <$> absSeekThenDo off m

chaseListGivenLength :: Offset -> Integer -> ReadIO a -> ReadIO [a]
chaseListGivenLength loff len m = do
  (num,noff) <- seekThenChars loff (liftA2 (,) readWord readWord)
  -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex noff)
  forM (take num $ [0,len..]) $
    \off2 -> absSeekThenDo (noff+off2) m

chaseListPointer :: forall a . MyLength a => Offset -> ReadIO a -> ReadIO [a]
chaseListPointer loff m = chaseListGivenLength loff (myLength (undefined :: a)) m

chaseListSimple :: Int -> Offset -> Integer -> ReadIO a -> ReadIO [a]
chaseListSimple num loff len m = do
  noff <- seekThenChars loff readWord
  -- liftIO $ hPutStrLn stderr $ "Maximum overdrive to " ++ show (Hex noff)
  forM (take num $ [0,len..]) $
    \off2 -> absSeekThenDo (noff+off2) m

  

readGoal :: CharIO Goal
readGoal = pure Goal 
  <*> readChars 
  <*> (bytesToGoalType <$> readChars)

readString :: CharIO String
readString = readChar >>= \case
  '\0' -> return []
  c -> fmap (c:) readString

readListOffset :: Offset -> CharIO ListOffset
readListOffset initOffset = pure ListOffset
  <*> readWord
  <*> fmap (\off -> off - initOffset) readWord

chaseAndReadTriIndexList :: ReadIO [Word16]
chaseAndReadTriIndexList = fmap (maybe [] id) $ chasePointerNullable 0x0 $ syncThenChars $
  fix $ \loop -> readHalf >>= \case
    0xffff -> return []
    ind -> (ind:) <$> loop

readCollisionHeader :: GlobalOffsets -> ReadIO CollisionHeader
readCollisionHeader (GlobalOffsets off1 off2 off3 off4 off5 off6 off7 off8 off9) = do
  -- Calculate triangle stuff
  numTriLists <- seekThenChars 0x34 $ pure (*) <*> readWord <*> readWord
  triIndexLists <- chaseListSimple numTriLists 0x20 0x4 chaseAndReadTriIndexList
  let numTris = fromIntegral $ maximum $ (0:) $ map (+1) $ concat triIndexLists -- map (+1) is where it is to handle null case
  tris <- chaseListSimple numTris 0x1c (myLength (undefined :: Triangle)) $ syncThenChars readChars

  pure CollisionHeader
    <*> seekThenChars 0x0 (liftM3 (,,) readFloat readFloat readFloat)
    <*> seekThenChars 0xc (liftM3 (,,) readHalf readHalf readHalf)
    <*> seekThenChars 0x12 readHalf
    <*> fmap (maybe (AnimData [] [] [] [] [] []) id) (chasePointerNullable 0x14 readAnimHeader)
    <*> fmap (maybe 0 id) (chasePointerNullable 0x14 getAnimLoopTime)
    <*> pure tris
    <*> pure triIndexLists
    <*> seekThenChars 0x24 readChars
    <*> seekThenChars 0x3c (readListOffset off1) -- Goals
    <*> seekThenChars 0x4c (readListOffset off2) -- Bumpers
    <*> seekThenChars 0x54 (readListOffset off3) -- Jamabars
    <*> seekThenChars 0x5c (readListOffset off4) -- Bananas
    <*> seekThenChars 0x64 (readListOffset off5) -- Cones
    <*> seekThenChars 0x6c (readListOffset off6) -- Spheres
    <*> seekThenChars 0x74 (readListOffset off7) -- Cylinders
    <*> seekThenChars 0x7c (readListOffset off8) -- Level Models
    <*> seekThenChars 0x8c (readListOffset off9) -- Reflective Models. docs seem to imply it should be 0xa4, but that's wrong

readAnimFrame :: CharIO AnimFrame
readAnimFrame = liftM4 AnimFrame readChars readFloat readFloat readChars

traverseAnimHeader :: (a -> a -> a -> a -> a -> a -> b) -> (Offset -> ReadIO a) -> ReadIO b
traverseAnimHeader f k = 
  pure f
    <*> k 0
    <*> k 8
    <*> k 16
    <*> k 24
    <*> k 32
    <*> k 40

readAnimHeader = traverseAnimHeader AnimData $ flip chaseListPointer (syncThenChars readAnimFrame)

getAnimLoopTime = traverseAnimHeader mex act 
  where
    act loff = chaseListGivenLength loff 0x14 $ fmap hexToFloat $ seekThenChars 0x4 readWord
    mex l1 l2 l3 l4 l5 l6 = maximum $ (0:) $ l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ l6
  
readSMB1 :: ReadIO LZData
readSMB1 = 
  getGlobalOffsets >>= \globalOffsets -> absSeekThenDo 0x0 $
    pure LZData
    <*> chaseListPointer 0x8 (readCollisionHeader globalOffsets)
    <*> chasePointer 0x10 (syncThenChars readChars)
    <*> chasePointer 0x14 (syncThenChars readChars)
    <*> chaseListPointer 0x18 (syncThenChars readGoal)
    <*> chaseListPointer 0x28 (syncThenChars readChars) -- bumpears
    <*> chaseListPointer 0x30 (syncThenChars readChars) -- jamabears
    <*> chaseListPointer 0x38 (syncThenChars readChars) -- BA NA NA
    <*> chaseListPointer 0x40 (syncThenChars readChars) -- Cones
    <*> chaseListPointer 0x48 (syncThenChars readChars) -- Spheres
    <*> chaseListPointer 0x50 (syncThenChars readChars) -- Cylinders
    <*> fmap (map getLevelModelEntry)       (chaseListPointer 0x58 $ fmap LevelModelEntry $ chasePointer 0x4 (syncThenChars readString))
    <*> fmap (map getReflectiveModelEntry)  (chaseListPointer 0x80 $ fmap ReflectiveModelEntry $ chasePointer 0x0 (syncThenChars readString))
