
{-# LANGUAGE FlexibleContexts, LambdaCase, MonoLocalBinds, TupleSections #-}

module WriteSMB2 (writeSMB2) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Function
import Data.List
import Data.Word
import System.IO

import Nat
import Types
import Vector
import WriteCommon

------ HELPERS ------

goalTypeToSMB2Bytes :: GoalType -> Vector Nat2 Char
goalTypeToSMB2Bytes BlueG = Cons '\x00' $ Cons '\x01' Nil
goalTypeToSMB2Bytes GreenG = Cons '\x01' $ Cons '\x01' Nil
goalTypeToSMB2Bytes RedG = Cons '\x02' $ Cons '\x01' Nil

wrByte :: Integer -> FileIO ()
wrByte = writeByte

wrHalf :: Integer -> FileIO ()
wrHalf = writeHalf

wrWord :: Integer -> FileIO ()
wrWord = writeWord

align :: FileIO ()
align = tell >>= \off -> replicateM_ (fromIntegral $ (-off) `mod` 4) $ writeChar '\0'

collisionHeaderOffset = 0x8b4


------ INITIAL WRITING PHASE ------
-- Notice that these writing helpers don't write offsets. That comes later, in the fixing phase

writeFileHeader :: LZData -> FileIO ()
writeFileHeader lzData = do
  wrWord 0x0
  wrWord 0x447a0000
  writeWord (length $ _collisionHeaders lzData)
  wrWord 0x8b4
  wrWord 0x89c
  wrWord 0x8b0
  replicateM 0x54 $ wrByte 0x0
  wrWord 0x1
  replicateM_ 0x82c $ wrByte 0x0

writeCollisionHeader :: CollisionHeader -> FileIO ()
writeCollisionHeader hdr = do
  _rotCenter hdr & \(cx,cy,cz) -> do
    writeFloat cx
    writeFloat cy
    writeFloat cz
  _initRot hdr & \(rx,ry,rz) -> do
    writeHalf rx
    writeHalf ry
    writeHalf rz
  writeHalf $ _animType hdr
  wrWord 0x0 -- Anim header pointer
  replicateM 3 $ wrWord 0x0 -- Conveyor speed
  wrWord 0x0 -- Tri list pointer
  wrWord 0x0 -- Index list pointer
  writeChars $ _collisionParameters hdr
  replicateM 0x90 $ writeChar '\x0'
  writeFloat $ _animLoopTime hdr
  replicateM_ 0x3c4 $ writeChar '\x0'

writeTriangleList :: [Triangle] -> FileIO Offset
writeTriangleList tris = tell <* mapM_ writeChars tris

writeIndexPointerList :: [[Word16]] -> FileIO Offset
writeIndexPointerList lss = tell <* replicateM (length lss) (wrWord 0x0)

writeIndexLists :: [[Word16]] -> FileIO [Offset]
writeIndexLists = mapM $ \indexList -> tell <* do
  mapM_ writeHalf indexList 
  wrHalf 0xffff
  align
  
writeGoals :: [Goal] -> FileIO Offset
writeGoals gs = tell <* do
  forM_ gs $ \(Goal a ty) -> writeChars a >> writeChars (goalTypeToSMB2Bytes ty)

writeObjects :: FixedReplicateA n => [Vector n Char] -> FileIO Offset
writeObjects objs = tell <* mapM_ writeChars objs
  
writeLevelAs :: [LevelModel] -> FileIO Offset
writeLevelAs strs = tell <* replicateM (length strs) `id` do
  wrWord 0x0 -- Yell at me for transparency/reflectivity stuff
  wrWord 0x1
  wrWord 0x0

writeLevelBs :: [LevelModel] -> FileIO Offset
writeLevelBs strs = tell <* replicateM (length strs) (wrWord 0x0)

writeLevelModels :: [LevelModel] -> FileIO Offset
writeLevelModels strs = tell <* replicateM (length strs) (replicateM 4 $ wrWord 0x0)

writeString :: String -> FileIO Offset
writeString str = tell <* do
  ReaderT $ flip hPutStr str
  writeChar '\x0'
  align

writeStrings :: [String] -> FileIO [Offset]
writeStrings = mapM writeString

writeReflectiveModels :: [String] -> FileIO Offset
writeReflectiveModels strs = tell <* replicateM (length strs) (replicateM 3 $ wrWord 0x0)

writeAnimHeaders :: [a] -> FileIO Offset
writeAnimHeaders ls = tell <* replicateM (length ls) (replicateM 16 $ wrWord 0x0)

writeAnimKeyframesForOne :: AnimData -> FileIO Offset6
writeAnimKeyframesForOne (AnimData rX rY rZ pX pY pZ) = do
  let
    writeFrame (AnimFrame e t v) = do
      writeChars e
      writeFloat t
      writeFloat v
      replicateM 0x8 (writeChar '\x0')
    writeOneSet frs = tell <* mapM_ writeFrame frs
  pure (,,,,,)
    <*> writeOneSet rX
    <*> writeOneSet rY
    <*> writeOneSet rZ
    <*> writeOneSet pX
    <*> writeOneSet pY
    <*> writeOneSet pZ

writeAnimKeyframesForAll :: LZData -> FileIO [Offset6]
writeAnimKeyframesForAll = mapM writeAnimKeyframesForOne . map _animData . _collisionHeaders


------ FIXING POINTERS PHASE ------

fixListOffsetNormal :: [a] -> Offset -> FileIO ()
fixListOffsetNormal ls off = writeWord (length ls) >> writeWord off

fixListOffsetForCollHeader :: Offset -> ListOffset -> FileIO ()
fixListOffsetForCollHeader off (ListOffset num relOff) 
  = writeWord num >> writeWord (off+relOff)

-- Counterpart to ReadSMB1's chaseListSimple
fixList :: Offset -> Integer -> [a] -> (a -> PlaceIO ()) -> FileIO ()
fixList loff len ls k = do
  forM_ (zip ls [0,len..]) $
    \(a,off2) -> do
      runReaderT (syncHandle >> k a) (loff+off2) 


fixFileHeader :: LZData -> WriteOffsets -> FileIO ()
fixFileHeader lzData writeOffsets = flip runReaderT 0x0 $ do
  seekAndDo 0x18 $ fixListOffsetNormal (_goals lzData) (_goalOffset writeOffsets)
  seekAndDo 0x20 $ fixListOffsetNormal (_bumpers lzData) (_bumperOffset writeOffsets)
  seekAndDo 0x28 $ fixListOffsetNormal (_jamabars lzData) (_jamabarOffset writeOffsets)
  seekAndDo 0x30 $ fixListOffsetNormal (_bananas lzData) (_bananaOffset writeOffsets)
  seekAndDo 0x70 $ fixListOffsetNormal (_reflectiveModels lzData) (_reflectiveModelOffset writeOffsets)
  seekAndDo 0x8c $ fixListOffsetNormal (_levelModels lzData) (_levelAOffset writeOffsets)
  seekAndDo 0x94 $ fixListOffsetNormal (_levelModels lzData) (_levelBOffset writeOffsets)

fixCollisionHeaders :: LZData -> WriteOffsets -> FileIO ()
fixCollisionHeaders lzData writeOffsets = do
  let 
    pairsOfOffsets = zip4
      (_collisionHeaders lzData)
      [_animHeaderOffset writeOffsets,_animHeaderOffset writeOffsets+0x40..] 
      (_collisionTriangleOffsets writeOffsets)
      (_indexPointerListOffsets writeOffsets)
  fixList collisionHeaderOffset 0x49c pairsOfOffsets $
    \(collHdr,animHdrOff,collTriOff,indexPointerListOff) -> do
      seekAndDo 0x14 $ writeWord animHdrOff
      seekAndDo 0x24 $ writeWord collTriOff
      seekAndDo 0x28 $ writeWord indexPointerListOff
      seekAndDo 0x44 $ fixListOffsetForCollHeader (_goalOffset writeOffsets) (_goalListOffset collHdr)
      seekAndDo 0x4c $ fixListOffsetForCollHeader (_bumperOffset writeOffsets) (_bumperListOffset collHdr)
      seekAndDo 0x54 $ fixListOffsetForCollHeader (_jamabarOffset writeOffsets) (_jamabarListOffset collHdr)
      seekAndDo 0x5c $ fixListOffsetForCollHeader (_bananaOffset writeOffsets) (_bananaListOffset collHdr)
      seekAndDo 0x84 $ fixListOffsetForCollHeader (_reflectiveModelOffset writeOffsets) (_reflectiveModelListOffset collHdr)
      seekAndDo 0x94 $ fixListOffsetForCollHeader (_levelBOffset writeOffsets) (_levelModelListOffset collHdr)
      
fixIndexPointerLists :: WriteOffsets -> FileIO ()
fixIndexPointerLists writeOffsets = do
  forM_ (liftM2 zip _indexPointerListOffsets _indexListOffsets writeOffsets) $ 
    \(indPtrLsOff,indLsOffs) -> flip runReaderT indPtrLsOff $ do
      syncHandle
      mapM_ (lift . writeWord) indLsOffs

fixLevelAs :: LZData -> WriteOffsets -> FileIO ()
fixLevelAs lzData writeOffsets = do
  let
    lvlAOff = _levelAOffset writeOffsets
    lvlMdlOff = _levelModelOffset writeOffsets
    numLvlMdls = length $ _levelModels lzData
  forM_ (take numLvlMdls $ zip [lvlAOff,lvlAOff+0xc..] [lvlMdlOff,lvlMdlOff+0x10..]) $
    \(aoff,moff) -> flip runReaderT aoff $ seekAndDo 0x8 $ writeWord moff
      
fixLevelBs :: LZData -> WriteOffsets -> FileIO ()
fixLevelBs lzData writeOffsets = do
  let
    lvlBOff = _levelBOffset writeOffsets
    lvlAOff = _levelAOffset writeOffsets
    numLvlMdls = length $ _levelModels lzData
  forM_ (take numLvlMdls $ zip [lvlBOff,lvlBOff+0x4..] [lvlAOff,lvlAOff+0xc..]) $
    \(boff,aoff) -> flip runReaderT boff $ seekAndDo 0x0 $ writeWord aoff
    
fixLevelModels :: WriteOffsets -> FileIO ()
fixLevelModels writeOffsets = do
  let
    lvlMdlOff = _levelModelOffset writeOffsets
    lvlMdlNameOffs = _levelNameOffsets writeOffsets
  forM_ (zip [lvlMdlOff,lvlMdlOff+0x10..] lvlMdlNameOffs) $
    \(moff,noff) -> flip runReaderT moff $ seekAndDo 0x4 $ writeWord noff

fixReflectiveModels :: WriteOffsets -> FileIO ()
fixReflectiveModels writeOffsets = do
  let
    reflMdlOff = _reflectiveModelOffset writeOffsets
    reflMdlNameOffs = _reflectiveNameOffsets writeOffsets
  forM_ (zip [reflMdlOff,reflMdlOff+0xc..] reflMdlNameOffs) $
    \(moff,noff) -> flip runReaderT moff $ seekAndDo 0x0 $ writeWord noff

fixAnimHeaders :: LZData -> WriteOffsets -> FileIO ()
fixAnimHeaders lzData writeOffsets = do
  let
    animHdrOff = _animHeaderOffset writeOffsets
    animKFOffs = _animKeyframeOffsets writeOffsets
    animDatas = map _animData $ _collisionHeaders lzData
  forM_ (zip3 [animHdrOff,animHdrOff+0x40..] animKFOffs animDatas) $
    \(hoff, (rxoff,ryoff,rzoff,pxoff,pyoff,pzoff), AnimData rx ry rz px py pz) -> 
      flip runReaderT hoff $ do
        let fixKFList roff ls off = seekAndDo roff $ fixListOffsetNormal ls off
        fixKFList 0x0 rx rxoff
        fixKFList 0x8 ry ryoff
        fixKFList 0x10 rz rzoff
        fixKFList 0x18 px pxoff
        fixKFList 0x20 py pyoff
        fixKFList 0x28 pz pzoff
        


------ THE ------

writeSMB2 :: LZData -> FileIO ()
writeSMB2 lzData = do
  let collHdrs = _collisionHeaders lzData
  -- Initial write phase 
  writeOffsets <- pure WriteOffsets
    <*  writeFileHeader lzData
    <*  writeChars (_startPos lzData)
    <*  writeChars (_falloutY lzData)
    <*  mapM writeCollisionHeader collHdrs
    <*> mapM writeTriangleList (map _triangles collHdrs)
    <*> mapM writeIndexPointerList (map _triangleIndexLists collHdrs)
    <*> mapM writeIndexLists (map _triangleIndexLists collHdrs)
    <*> writeGoals (_goals lzData)
    <*> writeObjects (_bumpers lzData)
    <*> writeObjects (_jamabars lzData)
    <*> writeObjects (_bananas lzData)
    <*> writeLevelAs (_levelModels lzData)
    <*> writeLevelBs (_levelModels lzData)
    <*> writeLevelModels (_levelModels lzData)
    <*> writeStrings (_levelModels lzData)
    <*> writeReflectiveModels (_reflectiveModels lzData)
    <*> writeStrings (_reflectiveModels lzData)
    <*> writeAnimHeaders (map _animData collHdrs)
    <*> writeAnimKeyframesForAll lzData
  -- Fix phase
  fixFileHeader lzData writeOffsets
  fixCollisionHeaders lzData writeOffsets
  fixIndexPointerLists writeOffsets
  fixLevelAs lzData writeOffsets
  fixLevelBs lzData writeOffsets
  fixLevelModels writeOffsets
  fixReflectiveModels writeOffsets
  fixAnimHeaders lzData writeOffsets
