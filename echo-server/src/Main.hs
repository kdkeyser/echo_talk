import System.IO
import Network
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.String

main :: IO () 
main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 6000
  handler sock
 
handler :: Socket -> IO () 
handler s = do
  (h,_,_) <- accept s
  hSetBuffering h NoBuffering
  forkIO $ echo h
  handler s

exit :: BS.ByteString
exit = fromString "exit\r"
 
echo :: Handle -> IO ()
echo h = do
  text <- BS.hGetLine h
  if text == exit then hClose h
  else do BS.hPutStrLn h text
          echo h
