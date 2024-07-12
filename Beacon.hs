{-# LANGUAGE OverloadedStrings, DeriveGeneric, MagicHash #-}
module Beacon where

import Generic
import Admin

import Data.Aeson
import Data.Aeson.Key as DAK
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM as S
import qualified Control.Exception as E
import Network.Socket
import qualified Network.Socket.Address as NA
import Network.Socket.ByteString
import Data.Text as T
import Data.Set as S
import Data.Word as W
import qualified Data.Vector as V
import System.Posix.Time
import System.Posix.Types
import Foreign.C.Types
import GHC.Int
import GHC.Word
import GHC.Exts
import GHC.Prim
import GHC.Generics
import Data.ByteString as B
import Data.Serialize as DS
import Data.Text.Encoding as TE

data Protocol = Protocol {
    -- 0 for query, 1 for answer, the rest reserved
    pmsgType :: Int8,
    psesId :: Int32,
    -- Text cannot be serialized
    pname :: B.ByteString,
    paddr :: Maybe HostAddress6
} deriving (Generic)
instance Serialize Protocol

data Session = Session {
    ssesId :: Int32,
    saddr :: Maybe HostAddress6,
    -- No upstream means we are the initiator
    upstm :: Maybe HostAddress6,
    sname :: Text,
    -- Session time to live, avoiding flood
    sttl :: Int
} deriving (Show)

defaultSession :: Int32 -> Session
defaultSession n = Session n Nothing Nothing "" 0

instance Eq Session where
    (==) :: Session -> Session -> Bool
    (==) s0 s1 = ssesId s0 == ssesId s1

instance Ord Session where
    compare :: Session -> Session -> Ordering
    compare s0 s1 = compare (ssesId s0) (ssesId s1)

newState :: IO State
newState = do
    x0 <- e
    x1 <- e
    x2 <- e
    x3 <- e
    x4 <- e
    State x0 x1 x2 x3 x4 <$> e
    where e = newEmptyTMVarIO

doQuery :: State -> Session -> IO ()
doQuery st ss = do
    s <- socket AF_INET6 Datagram defaultProtocol
    tree <- atomically $ readTMVar (tree st)
    let addrs = getSockAddr <$> S.toList tree
    forM_ addrs (sendBS phrase s) where
        -- For the socket, use sessionId for flowinfo!
        -- What about the scopeID? Let it be 0... 
        getSockAddr :: HostAddress6 -> SockAddr
        getSockAddr a = SockAddrInet6 internPort (int32ToWord32 . ssesId $ ss) a 0
        int32ToWord32 :: Int32 -> Word32
        int32ToWord32 (I32# i#) = W32# (int32ToWord32# i#)
        phrase :: B.ByteString
        phrase = DS.encode $ Protocol 0 (ssesId ss) (TE.encodeUtf8 . sname $ ss) Nothing
        sendBS :: B.ByteString -> Socket -> SockAddr -> IO ()
        sendBS b s a = sendTo s b a >> close s

doResponse :: State -> Session -> IO ()
doResponse = undefined

-- On query, we create a new session if not expired
-- On answer, we simply fill in the address
-- THE MAIN LOOP SHOULD ADD EXPIRED FLAG BEFORE REMOVING THE DEAD SESSION!
-- The address should be derived from getSelf

serverThread :: HostAddress6 -> TMVar (Set Session) -> State -> IO ()
serverThread a ss st = do
    s <- socket AF_INET6 Datagram defaultProtocol
    bind s $ getSockAddr a
    serverLoop s where
        getSockAddr :: HostAddress6 -> SockAddr
        getSockAddr a = SockAddrInet6 internPort 0 a 0
        handleDgram :: B.ByteString -> HostAddress6 -> IO ()
        handleDgram b src = do
            case p of
                Left str -> putStrLn str
                Right p' -> case pmsgType p' of
                    0 -> onQuery p'
                    1 -> onResp p'
                    _ -> print "WTF are u sending 2 me?"
            where
                p :: (Either String Protocol) = DS.decode b
                onQuery :: Protocol -> IO ()
                onQuery p = do
                    se <- atomically $ readTMVar (sesExpir st)
                    unless (member (psesId p) se) (atomically $ do
                        ss' <- readTMVar ss 
                        writeTMVar ss $ insert newSession ss')  where
                        newSession = Session (psesId p) Nothing Nothing (decodeUtf8 . pname $ p) defaultSessionTTL
                onResp :: Protocol -> IO ()
                onResp p = do
                    let a = paddr p
                    case a of 
                        Nothing -> print "No address provided in response package, why?"
                        Just a' -> do
                            se <- atomically $ readTMVar (sesExpir st)
                            unless (member (psesId p) se) (atomically $ do  
                                ss' <- readTMVar ss 
                                -- Will this work?
                                let u = lookupGE (defaultSession . psesId $ p) ss' 
                                case u of 
                                    Nothing -> pure ()
                                    -- No check on name here! Can be poisoned!
                                    Just sess -> when (ssesId sess == psesId p) 
                                        (writeTMVar ss $ insert (sess { saddr = Just a', sttl = sttl sess - 1 }) ss' ))
        serverLoop :: Socket -> IO ()
        serverLoop s = do
            (buf, SockAddrInet6 _ _ src _) <- NA.recvFrom s (fromIntegral maxRecvLength)
            handleDgram buf src
            serverLoop s

-- Workthread should work on state and sessions list
-- There should be a mechanism for worker to notify upstream/initiator...
workerThread :: a
workerThread = undefined


refreshGeneralPeriodic :: (Ord a) => (Record -> Maybe a) -> IO (V.Vector Record) -> TMVar (Set a) -> TMVar Int64 -> IO ()
refreshGeneralPeriodic trans populate r t = do
    x <- V.toList <$> populate
    let Just x' = foldNothing $ trans <$> x
    foldM_ insertSTM r x'
    updateTime
    where
        insertSTM :: (Ord a) => TMVar (Set a) -> a -> IO (TMVar (Set a))
        insertSTM l y = atomically $ do
            l' <- readTMVar l
            writeTMVar l . insert y $ l'
            pure l -- Just for the type system to be happy
        updateTime :: IO ()
        updateTime = do
            CTime t' <- epochTime
            atomically $ writeTMVar t (t' + refreshPeriod)


refreshNamePeriodic :: TMVar (Set NS6Record) -> TMVar Int64 -> IO ()
refreshNamePeriodic = refreshGeneralPeriodic recordToNS6Record populateNames

refreshTreePeriodic :: TMVar (Set HostAddress6) -> TMVar Int64 -> IO ()
refreshTreePeriodic = refreshGeneralPeriodic (addr6FromText . address) populateTree

refreshBlockedSession :: TMVar (Set Int32) -> TMVar Int64 -> IO ()
refreshBlockedSession r t = do
    atomically $ writeTMVar r S.empty
    CTime t' <- epochTime
    atomically $ writeTMVar t (t' + refreshSessionsPeriod)

-- At the call time states should be empty MVars
refreshThread :: State -> IO ()
refreshThread s = do
    -- Rewrite this ugly block later
    atomically $ putTMVar (names s) S.empty
    atomically $ putTMVar (tree s) S.empty
    atomically $ putTMVar (sesExpir s) S.empty
    atomically $ putTMVar (nT s) 0
    atomically $ putTMVar (tT s) 0
    atomically $ putTMVar (sT s) 0
    doRefresh
    where
        delayTime = 500 * 1000
        initialization :: IO ()
        initialization = undefined
        doRefresh :: IO ()
        doRefresh = do
            CTime ct <- epochTime

            nt <- atomically $ readTMVar (nT s)
            when (ct >= nt) $ refreshNamePeriodic (names s) (nT s)

            tt <- atomically $ readTMVar (tT s)
            when (ct >= tt) $ refreshTreePeriodic (tree s) (tT s)

            st <- atomically $ readTMVar (tT s)
            when (ct >= st) $ refreshBlockedSession (sesExpir s) (sT s)

            threadDelay delayTime
            doRefresh

debugStates :: State -> IO ()
debugStates s = do
    atomically (readTMVar (names s)) >>= print
    atomically (readTMVar (tree s)) >>= print

