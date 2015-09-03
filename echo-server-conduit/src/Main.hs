{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Data.ByteString
import           Data.Conduit.Network
import           Data.Conduit.Binary

echo :: Conduit ByteString IO ByteString
echo = do
    l <- await
    case l of
      Nothing -> return ()
      Just "exit\r" -> return ()
      Just s -> do
                  yield $ Data.ByteString.append s "\n"
                  echo

main :: IO ()
main = runTCPServer (serverSettings 6000 "*") $ \appData ->
    appSource appData
    =$= Data.Conduit.Binary.lines
    =$= echo
    $$ appSink appData
