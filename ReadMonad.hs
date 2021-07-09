
{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeOperators #-}

-- Simple effect system for reading binary files

module ReadMonad (ReadIO, CharIO, getChr, syncThenChars, modThenDo, modThenChars, seekThenDo, seekThenChars, absSeekThenDo, absSeekThenChars, runReadIO) where


import GHC.Generics ((:+:)(..))
import System.IO

import CommonEffects
import SuperF
import Vector


data GetCharL a where
  GetChar :: GetCharL Char

type GetCharEndo f = GetCharL :+: f

type CharIOL = GetCharEndo (DieEndo Void1)
type ReadIOL = SyncHandleEndo CharIOL

newtype CharIO a = CharIO (SuperF CharIOL a)
  deriving (Functor, Applicative, Monad, Prnt, Abrt)
newtype ReadIO a = ReadIO (SuperF ReadIOL a)
  deriving (Functor, Applicative, Monad, Prnt, Abrt)

getChr :: CharIO Char
getChr = CharIO (send GetChar)

syncThenChars :: CharIO a -> ReadIO a
syncThenChars (CharIO m) = ReadIO $ syncHandle >> liftSF m

modThenDo :: (Integer -> Integer) -> ReadIO a -> ReadIO a
modThenDo md (ReadIO m) = ReadIO (lcal md m)

modThenChars :: (Integer -> Integer) -> CharIO a -> ReadIO a
modThenChars md m = modThenDo md (syncThenChars m)

seekThenDo :: Integer -> ReadIO a -> ReadIO a
seekThenDo i = modThenDo (+i)

seekThenChars :: Integer -> CharIO a -> ReadIO a
seekThenChars i m = seekThenDo i (syncThenChars m)

absSeekThenDo :: Integer -> ReadIO a -> ReadIO a
absSeekThenDo i = modThenDo (const i)

absSeekThenChars :: Integer -> CharIO a -> ReadIO a
absSeekThenChars i m = absSeekThenDo i (syncThenChars m)

getCharWr :: Wr GetCharL (Handle -> IO a)
getCharWr GetChar k handle = hGetChar handle >>= \c -> k c handle

readIOWr :: Wr ReadIOL (Handle -> IO a)
readIOWr = combineWr (readerWr 0) $ combineWr seekHandleWr $ combineWr getCharWr $ combineWr printWr $ combineWr abortWr voidWr

runReadIO :: ReadIO a -> Handle -> IO a
runReadIO (ReadIO sf) = runSuperF sf (\a _ -> return a) readIOWr 
