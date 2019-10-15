
import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Text.Read

main = do
  args <- getArgs
  when (length args /= 2) $
    die $ "Usage: ./Pad [nulls to add] [file name]"
  let [numString,fileName] = args

  let numMaybe = (readMaybe numString :: Maybe Int)
  num <- case numMaybe of
    Nothing -> die $ "Number parameter isn't parsable"
    Just a -> return a

  handle <- openFile fileName ReadWriteMode
  hSeek handle SeekFromEnd 0x0
  replicateM num $ hPutChar handle '\x0'

  hFlush handle
  hClose handle
