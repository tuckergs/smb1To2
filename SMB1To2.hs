
module Main where

import Control.Monad
import Control.Monad.Reader
import System.Environment
import System.Exit
import System.IO

import FixDiffs1To2
import ReadMonad
import ReadSMB1
import WriteSMB2


main = do
  args <- getArgs
  when (length args /= 2) $
    die $ "Usage: ./SMB1To2 [in raw] [out raw]"
  let (inFileName:outFileName:[]) = args

  inFile <- openBinaryFile inFileName ReadMode
  outFile <- openBinaryFile outFileName WriteMode

  runReadIO readSMB1 inFile >>= return . fixDiffs >>= flip runReaderT outFile . writeSMB2

  hClose inFile
  hClose outFile
