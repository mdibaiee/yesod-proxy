{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}

import Yesod
import Debug.Trace
import Data.Maybe
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai
import Network.Wai.Conduit
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Blaze.ByteString.Builder (fromByteString)
import Data.List
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (ok200)
import Network.Socket as S
import Network.Socket.ByteString as SB
import System.IO
import Control.Monad (when)
import qualified Control.Concurrent.Thread as T
import qualified Data.Conduit as C

data App = App

mkYesod "App" [parseRoutes|
/*Texts ProxyR
|]

instance Yesod App

handleProxyR :: Texts -> HandlerT App IO TypedContent
handleProxyR _ = do
  manager <- liftIO $ H.newManager tlsManagerSettings
  request <- waiRequest
  if requestMethod request == "CONNECT"
    then connectProxyR
    else do
      response <- liftIO $ H.responseOpen (translateRequest request) manager
      let status = H.responseStatus response
          headers = H.responseHeaders response
          contentType = maybe "plain/text" snd $
                              find (\(k, v) -> k == hContentType) headers

      respondSource contentType $ do
        sendFlush

        let loop =
              do
                bs <- liftIO $ H.brRead $ H.responseBody response

                if BS.null bs
                  then do
                    liftIO $ H.responseClose response
                    sendFlush
                  else do
                    sendChunkBS bs
                    sendFlush
                    loop
        loop

connectProxyR :: HandlerT App IO TypedContent
connectProxyR = do
  manager <- liftIO $ H.newManager tlsManagerSettings
  request <- waiRequest
  let requestStream = sourceRequestBody request

  let host = fromJust $ requestHeaderHost request
      (hostname, port) = span (/= ':') (BSC.unpack host)

  targetSocket <- liftIO $ openSocket hostname (tail port)

  sendWaiResponse $ responseStream ok200 [] $ \write flush -> do
    (rid, rwait) <- liftIO $ T.forkIO $
      do
        let loop = do
              output <- liftIO $ SB.recv targetSocket 128
              liftIO $ putStrLn $ "output: " ++ show output

              when (not $ BS.null output) $
                do
                  write $ fromByteString output
                  flush
                  loop
        loop
    (wid, wwait) <- liftIO $ T.forkIO $
      do
        let loop = do
              maybeInput <- requestStream C.$$ C.await

              when (isJust maybeInput) $
                do
                  let input = fromJust maybeInput
                  liftIO $ SB.sendAll targetSocket input
                  loop
        loop

    -- flush

    rwait
    wwait
    return ()

main = warp 3000 App

translateRequest req =
  H.defaultRequest {
    H.method = requestMethod req,
    H.secure = isSecure req,
    H.host = fromJust $ requestHeaderHost req,
    H.queryString = rawQueryString req,
    H.requestHeaders = requestHeaders req,
    H.path = rawPathInfo req
  }

openSocket :: HostName -> String -> IO Socket
openSocket hostname port = do
  addrinfo <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfo

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  setSocketOption sock KeepAlive 1

  connect sock (addrAddress serveraddr)

  return sock
