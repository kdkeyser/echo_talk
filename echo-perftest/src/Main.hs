import System.IO
import Network
import Control.Concurrent
import Control.Monad
import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as BS
import Data.String
import Debug.Trace

exampleMessage = BS.pack "TestMessage"
numberOfMessages = 1000000
numberOfConnections = 10

singleConnection :: IO ()
singleConnection = do
  handle <- connectTo "localhost" $ PortNumber 6000
  singleConnectionPerfTest handle

readAsFastAsPossible :: Handle -> IO ()
readAsFastAsPossible h = do
  replicateM_ numberOfMessages $ do
    l <- BS.hGetLine h
 --   BS.putStrLn l
    return l

singleConnectionPerfTest :: Handle -> IO ()
singleConnectionPerfTest h = do
  hSetBuffering h NoBuffering
  asyncReader <- async $ readAsFastAsPossible h
  replicateM_ numberOfMessages $ BS.hPutStrLn h exampleMessage
  wait asyncReader
  hClose h

main :: IO ()
main = withSocketsDo $ do
  threads <- replicateM numberOfConnections (async singleConnection)
  results <- mapM wait threads
  return ()

