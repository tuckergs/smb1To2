

import Control.Monad
import Control.Monad.Reader
import System.Environment
import System.Exit
import System.IO

import HexStuff
import Nat
import ReadSMB1
import Types
import Vector



putTabStrLn str = ReaderT $ \t -> putStrLn $ take (2*t) (repeat ' ') ++ str

addTab = local (+1)


main = do
  args <- getArgs
  when (length args /= 1) $
    die $ "Usage: ./ShowSMB1 [in raw]"
  let [inFileName] = args

  lzData <- openBinaryFile inFileName ReadMode >>= runReaderT readSMB1

  flip runReaderT 0 $ do
    putTabStrLn "LZ Schtuff:"
    addTab $ do
      putTabStrLn $ "Start pos: " ++ show (_startPos lzData)
      putTabStrLn $ "Fallout Y: " ++ show (_falloutY lzData)
      let 
        printList label val = do
          putTabStrLn label
          addTab $ forM_ val $ putTabStrLn . show
      printList "Goals:" (_goals lzData)
      printList "Bumpers:" (_bumpers lzData)
      printList "Jamabars:" (_jamabars lzData)
      printList "Bananas:" (_bananas lzData)
      printList "Cones:" (_cones lzData)
      printList "Spheres:" (_spheres lzData)
      printList "Cylinders:" (_cylinders lzData)
      printList "Level Models:" (_levelModels lzData)
      printList "Reflective Models:" (_reflectiveModels lzData)
      putTabStrLn "Collision Headers:"
      forM_ (_collisionHeaders lzData) $ \cl -> do
        putTabStrLn ""
        putTabStrLn $ "Collision Header:"
        addTab $ do
          putTabStrLn $ "Center of rotation:" ++ show (_rotCenter cl)
          putTabStrLn $ "Initial rotation:" ++ show (_initRot cl)
          putTabStrLn $ "Anim type:" ++ show (_animType cl)
          putTabStrLn $ "Collision Parameters: " ++ show (_collisionParameters cl)
          putTabStrLn $ "Goal list offset: " ++ show (_goalListOffset cl)
          putTabStrLn $ "Bumper list offset: " ++ show (_bumperListOffset cl)
          putTabStrLn $ "Jamabar list offset: " ++ show (_jamabarListOffset cl)
          putTabStrLn $ "Banana list offset: " ++ show (_bananaListOffset cl)
          putTabStrLn $ "Cone list offset: " ++ show (_coneListOffset cl)
          putTabStrLn $ "Sphere list offset: " ++ show (_sphereListOffset cl)
          putTabStrLn $ "Cylinder list offset: " ++ show (_cylinderListOffset cl)
          putTabStrLn $ "Level Model list offset: " ++ show (_levelModelListOffset cl)
          putTabStrLn $ "Reflective Model list offset: " ++ show (_reflectiveModelListOffset cl)
          putTabStrLn $ "Animation loop time:" ++ show (_animLoopTime cl)
          putTabStrLn $ "Animation data:"
          addTab $ do
            let an = _animData cl
            printList "Rot X Keyframes:" (_rotXFrames an)
            printList "Rot Y Keyframes:" (_rotYFrames an)
            printList "Rot Z Keyframes:" (_rotZFrames an)
            printList "Pos X Keyframes:" (_posXFrames an)
            printList "Pos Y Keyframes:" (_posYFrames an)
            printList "Pos Z Keyframes:" (_posZFrames an)


        
        
