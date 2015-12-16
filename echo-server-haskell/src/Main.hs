module Main where

import Network.Socket
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad.Loops
import Control.Monad
import Data.Word
import Debug.Trace
import Control.Exception.Base
import System.IO.Error
import Control.Concurrent

bufferSize = 16*4096

main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    setSocketOption sock NoDelay 1
    -- listen on TCP port 6000
    bindSocket sock (SockAddrInet 6000 iNADDR_ANY)
    -- allow a maximum of 5 outstanding connections
    listen sock 5000
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    -- accept one connection and handle it
    conn <- accept sock
    forkIO $ runConn conn
    mainLoop sock

singleRead :: Socket -> ForeignPtr a -> (Int,Int) -> IO (Int, Int)
singleRead socket buffer (offset,remainingSize) =
  withForeignPtr buffer $ \ptr -> do
    let offsetPtr = plusPtr ptr offset
    bytesRecv <- recvBuf socket offsetPtr remainingSize
    --putStrLn $ show bytesRecv
    return (offset+bytesRecv, remainingSize-bytesRecv)

readUntilNewline :: Socket -> ForeignPtr Word8 -> (Int,Int) -> IO Int
readUntilNewline socket buffer (offset, size) = do
  (nextOffset, remainingSize) <- singleRead socket buffer (offset,size)
  if (remainingSize <= 0) then
    return nextOffset
  else do
    value <- withForeignPtr buffer $ \ptr -> peekElemOff ptr (nextOffset-1)
    --putStrLn $ show value
    if (value == 10) then
      return nextOffset
    else
      readUntilNewline socket buffer (nextOffset, remainingSize)

singleSend :: Socket -> ForeignPtr a -> (Int,Int) -> IO (Int, Int)
singleSend socket buffer (offset,remainingSize) =
  withForeignPtr buffer $ \ptr -> do
    let offsetPtr = plusPtr ptr offset
    bytesSent <- sendBuf socket offsetPtr remainingSize
    return (offset+bytesSent, remainingSize-bytesSent)

sendAll :: Socket -> ForeignPtr a -> Int -> IO ()
sendAll socket buffer size = do
  _ <- iterateUntilM ( \(_,size) -> size <= 0) (singleSend socket buffer) (0,size)
  return ()

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
--    putStrLn "Connection established"
    setSocketOption sock NoDelay 1
    buffer <-  mallocForeignPtrBytes bufferSize
    catchJust (\e -> if isEOFErrorType (ioeGetErrorType e) then Just () else Nothing)
              (forever $ do
                           bytesRead <- readUntilNewline sock buffer (0, bufferSize)
                           sendAll sock buffer bytesRead)
              (\_ -> return ()) --putStrLn "Connection terminated")
    sClose sock
