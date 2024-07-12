{-# LANGUAGE OverloadedStrings #-}
module Admin (
    populateNames, populatePeers, populateTree,
    maxRecvLength,
) where

import Generic
import GHC.Generics

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import qualified Network.Socket.Address as NA
import Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Aeson.Key as DAK
import Data.Text as T
import Data.Text.Encoding as ET
import qualified Data.Text.Encoding as ET
import Data.Aeson.Types (parseMaybe)
import Data.Vector as V
import Data.Complex (phase)
import GHC.IO (unsafePerformIO)
import qualified Language.Haskell.TH.Syntax as BL

getTree :: Value
getTree = object [ ("request", "getTree") ]

getSelf :: Value
getSelf = object [ ("request", "getSelf") ]

getPaths :: Value
getPaths = object [ ("request", "getPaths") ]

getPeers :: Value
getPeers = object [ ("request", "getPeers") ]

getNodeInfo :: Text -> Value
getNodeInfo key = object [ ("request", String "getNodeInfo"), ("arguments", object [ ("key", String key) ])  ]

yggAdminAddr :: SockAddr
yggAdminAddr = SockAddrUnix "/var/run/yggdrasil/yggdrasil.sock"

queryAdminRaw :: Socket -> BL.ByteString -> IO BL.ByteString
queryAdminRaw s q = do
    sendAll s q
    recv s maxRecvLength

queryAdminSingle :: BL.ByteString -> IO BL.ByteString
queryAdminSingle q = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s yggAdminAddr
    E.bracket (pure s) close $ flip queryAdminRaw q

-- Useless 
parseResponse :: BL.ByteString -> Maybe Value
parseResponse r = do
    result <- decode r
    flip parseMaybe result $ \v0 -> do
        v0 .: "response"

parseSelfKey :: BL.ByteString -> Maybe Text
parseSelfKey r = do
    result <- decode r
    flip parseMaybe result $ \v0 -> do
        (Object v1 :: Value) <- v0 .: "response"
        (String v2 :: Value) <- v1 .: "key"
        pure v2

parseTreeNodes :: BL.ByteString -> Maybe (Vector Record)
parseTreeNodes r = do
    result <- decode r
    v2' <- flip parseMaybe result $ \v0 -> do
        (Object v1 :: Value) <- v0 .: "response"
        (Array  v2 :: Value) <- v1 .: "tree"
        pure v2
    forM v2' phase2Single
    where
        phase2Single :: Value -> Maybe Record
        phase2Single (Object o) = do
            (a', k') <- flip parseMaybe o $ \v0 -> do
                String a <- v0 .: "address"
                String k <- v0 .: "key"
                pure (a, k)
            aV <- addr6Validation a'
            kV <- pubkeyValidation k'
            pure $ Record aV kV Nothing
        phase2Single _ = Nothing

parsePeers :: BL.ByteString -> Vector Record
parsePeers r = case toPeerList of
    Nothing -> V.empty
    Just v -> mapMaybe parsePeerSingle v
    where
        toPeerList :: Maybe (Vector Value)
        toPeerList = do
            result <- decode r
            flip parseMaybe result $ \v0 -> do
                Object v1 <- v0 .: "response"
                Array  v2 <- v1 .: "peers"
                pure v2
        parsePeerSingle :: Value -> Maybe Record
        parsePeerSingle (Object o) = do
            (a', k') <- flip parseMaybe o $ \v0 -> do
                String a <- v0 .: "address"
                String k <- v0 .: "key"
                pure (a, k)
            a'' <- addr6Validation a'
            k'' <- pubkeyValidation k'
            pure $ Record a'' k'' Nothing

populateTree :: IO (Vector Record)
populateTree = do
    tree <- queryAdminSingle . encode $ getTree
    case parseTreeNodes tree of
        Just vt -> pure vt
        _ -> pure V.empty

populatePeers :: IO (Vector Record)
populatePeers = do
    peers <- queryAdminSingle . encode $ getPeers 
    pure $ parsePeers peers 

populateNames :: IO (Vector Record)
populateNames = do
    tree <- queryAdminSingle . encode $ getTree
    self <- queryAdminSingle . encode $ getSelf
    case parseSelfKey self of
        Nothing -> pure V.empty
        Just key -> case parseTreeNodes tree of
            Nothing -> pure V.empty
            Just records -> do
                catMaybes <$> forM records findName
                where
                    findName :: Record -> IO (Maybe Record)
                    findName r@(Record _ k _) = do
                        if k == key then pure Nothing else do
                            name <- queryAdminSingle . encode $ getNodeInfo k
                            case getNodeName name of
                                Nothing -> pure Nothing
                                Just nt -> pure . pure $ r { name = Just nt }
                            where
                                getNodeName :: BL.ByteString -> Maybe Text
                                getNodeName r = do
                                    result <- decode r
                                    flip parseMaybe result $ \v0 -> do
                                        Object v1 <- v0 .: "response"
                                        Object v2 <- v1 .: fromText k
                                        v2 .: "name"

resolveNames :: Vector Record -> IO (Vector Record)
resolveNames = undefined


