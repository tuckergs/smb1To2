
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeOperators #-}

module CommonEffects where

import GHC.Generics ((:+:)(..))
import System.Exit
import System.IO

import SuperF


data ReaderL e a where
  Get :: forall e e0. e ~ e0 => ReaderL e e0

gt :: Member (ReaderL e) ell => SuperF ell e
gt = send Get

lcalHalf :: (e -> e) -> Wr (ReaderL e) r -> ReaderL e x -> (x -> r) -> r
lcalHalf md wr Get g = wr Get (\x -> g (md x))

lcal :: Member (ReaderL e) ell => (e -> e) -> SuperF ell a -> SuperF ell a
lcal md m = interpose (lcalHalf md) m 

readerWr :: e -> Wr (ReaderL e) r
readerWr e Get k = k e

type GetPosL = ReaderL Integer
type GetPosEndo f = GetPosL :+: f


data SeekHandleL a where
  SeekHandle :: Integer -> SeekHandleL ()

seekHandleWr :: Wr SeekHandleL (Handle -> IO a)
seekHandleWr (SeekHandle pos) k handle = hSeek handle AbsoluteSeek pos >> k () handle


type SyncHandleL = GetPosL :+: SeekHandleL
type SyncHandleEndo f = GetPosL :+: SeekHandleL :+: f

syncHandle :: (Member GetPosL ell, Member SeekHandleL ell) => SuperF ell ()
syncHandle = gt >>= send . SeekHandle

syncHandleWr = combineWr (readerWr 0) seekHandleWr



data PrintHandles = StdOut | StdErr

data PrintL a where
  Print :: PrintHandles -> String -> PrintL ()

class Monad m => Prnt m where
  prnt :: PrintHandles -> String -> m ()
instance Member PrintL ell => Prnt (SuperF ell) where
  prnt handle msg = send (Print handle msg)

printWr :: Wr PrintL (b -> IO a)
printWr (Print StdOut msg) k b = hPutStrLn stdout msg >> k () b
printWr (Print StdErr msg) k b = hPutStrLn stderr msg >> k () b


data AbortL a where
  Abort :: AbortL a

class Monad m => Abrt m where
  abrt :: m a
instance Member AbortL ell => Abrt (SuperF ell) where
  abrt = send Abort

abortWr :: Wr AbortL (b -> IO a)
abortWr Abort k b = exitFailure >> k undefined b


type DieEndo f = PrintL :+: AbortL :+: f

dye :: (Prnt m, Abrt m) => String -> m a
dye msg = prnt StdErr msg >> abrt

dieWr = combineWr printWr abortWr
