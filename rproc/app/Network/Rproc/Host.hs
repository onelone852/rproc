{-# LANGUAGE OverloadedStrings #-}

module Network.Rproc.Host (
  -- Exported types
  Host,
  newSingleServiceHost,
  newMultiServicesHost,
  addService,
  -- Run
  run,
  -- Info functions
  getPort,
) where

import Data.Aeson (Object, Value (String), decode, encode, object, (.=))
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (status200, status400, status404)
import qualified Network.HTTP.Types.Status as Status
import Network.Rproc.Service (CallError, Service, call, toStatus)
import Network.Wai (ResponseReceived, rawPathInfo)
import qualified Network.Wai as Net
import qualified Network.Wai.Handler.Warp as Warp

data HostingService = SingleService Service | MultiServices (HM.HashMap BS.ByteString Service)

data Host = Host Int HostingService

getPort :: Host -> Int
getPort (Host p _) = p

getHostingService :: Host -> HostingService
getHostingService (Host _ hs) = hs

newSingleServiceHost :: Int -> Service -> Host
newSingleServiceHost port service = Host port (SingleService service)

newMultiServicesHost :: Int -> Host
newMultiServicesHost port = Host port (MultiServices HM.empty)

addService :: BS.ByteString -> Service -> Host -> Host
addService str service (Host port (MultiServices services)) = Host port (MultiServices (HM.insert str service services))
addService _ _ _ = error "addService: SingleServiceHost"

valueToString :: Value -> Maybe Text
valueToString (String str) = Just str
valueToString _ = Nothing

parser :: Maybe Object -> Maybe (Text, Value)
parser Nothing = Nothing
parser (Just obj) = do
  rawMethod <- obj !? "method"
  method <- valueToString rawMethod
  params <- obj !? "params"
  return (method, params)

headers :: ResponseHeaders
headers = [("Content-Type", "application/json")]

parseCallResult :: Either CallError (IO Value) -> IO (Status.Status, Value)
parseCallResult (Left err) = return (toStatus err, object ["error" .= show err])
parseCallResult (Right result) = (\res -> (status200, object ["result" .= res])) <$> result

callRespond :: (Status.Status -> ResponseHeaders -> ByteString -> IO ResponseReceived) -> (Text, Value) -> Service -> IO ResponseReceived
callRespond respond (method, params) service = do
  (status, callResult) <- parseCallResult $ call method params service
  respond status headers (encode callResult)

app :: HostingService -> Net.Application
app hs req rawRespondFn = do
  let rawRespond x y z = rawRespondFn $ Net.responseLBS x y z
  let respond = callRespond rawRespond
  let toJSON = encode . object

  body <- Net.lazyRequestBody req
  let request = decode body :: Maybe Object
  let parseResult = parser request
  case parseResult of
    Just result -> case hs of
      SingleService service -> respond result service
      MultiServices services -> do
        let path = rawPathInfo req
        case HM.lookup path services of
          Just service -> respond result service
          Nothing -> rawRespond status404 headers (toJSON ["error" .= ("Not Found Service" :: String)])
    Nothing -> rawRespond status400 headers (toJSON ["error" .= ("Parse failed" :: String)])

run :: Host -> IO ()
run host = Warp.run (getPort host) (app $ getHostingService host)
