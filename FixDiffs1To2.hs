
module FixDiffs1To2 (fixDiffs) where

import Control.Lens
import Types
import Vector

dummyCollisionHeader :: CollisionHeader
dummyCollisionHeader = CollisionHeader
  (0,0,0)
  (0,0,0)
  0
  (AnimData [] [] [] [] [] [])
  0
  []
  [[]]
  (listToVector $ take 16 (repeat '\x0') ++ ['\x0','\x0','\x0','\x1','\x0','\x0','\x0','\x1'])
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)
  (ListOffset 0x0 0x0)

fixKeyframes :: Float -> [AnimFrame] -> [AnimFrame]
fixKeyframes a [] = (:[]) $ 
  AnimFrame (Cons '\x0' $ Cons '\x0' $ Cons '\x0' $ Cons '\x1' Nil) 0 a
fixKeyframes a ls = ls
-- fixKeyframes a ls = flip map ls $ over value (+a)

fixKeyframesForCollHeader :: CollisionHeader -> CollisionHeader
fixKeyframesForCollHeader hdr =
  let 
    (cx,cy,cz) = view rotCenter hdr
    (rxi,ryi,rzi) = view initRot hdr
    shortToFloat = ((360/65536)*) . fromIntegral
    rx = shortToFloat rxi
    ry = shortToFloat ryi
    rz = shortToFloat rzi
  in 
    over (animData . posXFrames) (fixKeyframes cx)
    $ over (animData . posYFrames) (fixKeyframes cy)
    $ over (animData . posZFrames) (fixKeyframes cz) 
    $ over (animData . rotXFrames) (fixKeyframes rx)
    $ over (animData . rotYFrames) (fixKeyframes ry)
    $ over (animData . rotZFrames) (fixKeyframes rz) hdr

fixDiffs :: LZData -> LZData
fixDiffs = 
  over collisionHeaders (dummyCollisionHeader:)
  . over (collisionHeaders . traverse) fixKeyframesForCollHeader
  . over (collisionHeaders . traverse . reflectiveModelListOffset) (\(ListOffset a b) -> ListOffset a (3*(b `div` 2)))
  . over (collisionHeaders . traverse . levelModelListOffset)      (\(ListOffset a b) -> ListOffset a (b `div` 3))
