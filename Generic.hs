{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Generic (
    internPort, 
    maxRecvLength, refreshPeriod, refreshSessionsPeriod, defaultSessionTTL,
    pubkeyValidation, addr6Validation, addr6FromText,
    Record (Record), address, pubkey, name,
    NS6Record (NS6Record), ns6Address, ns6Name,
    State (State), names, tree, sesExpir, nT, tT, sT,
    recordToNS6Record,
    foldNothing
) where

import GHC.Generics
import qualified GHC.Int as GI
import Numeric
import Data.Word
import Data.Text as T
import Data.Serialize
import qualified Data.Vector as V
import Data.Map (Map)
import Data.Set
import Data.Aeson as A
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM as S
import GHC.Int

maxRecvLength :: GI.Int64
maxRecvLength = 65535

refreshPeriod :: GI.Int64
refreshPeriod = 5

refreshSessionsPeriod :: GI.Int64
refreshSessionsPeriod = 60

internPort :: PortNumber
internPort = 8965

defaultSessionTTL :: Int
defaultSessionTTL = 1

-- We may implement this check later...
pubkeyValidation :: Text -> Maybe Text
pubkeyValidation k = if T.length k == 64 then Just k else Nothing

-- We may implement this check later...
addr6Validation :: Text -> Maybe Text
addr6Validation = pure

l8ListToTuple :: [a] -> (a, a, a, a, a, a, a, a)
l8ListToTuple [x0, x1, x2, x3, x4, x5, x6, x7] = (x0, x1, x2, x3, x4, x5, x6, x7)

foldNothing :: [Maybe a] -> Maybe [a]
foldNothing [] = Just []
foldNothing (Just a : xs) = foldNothing xs >>= (\aa -> pure $ a : aa)
foldNothing _ = Nothing

-- This is not a complete validity check! It may not fail on , for example, ":::"
addr6FromText :: Text -> Maybe HostAddress6
addr6FromText s = do
    let ss0 = splitOn "::" s
    (ss1, ss1') <- case ss0 of
        [s'] -> Just (s', Nothing)
        [s', s''] -> Just (s', Just s'')
        _ -> Nothing
    let ss2 = splitOn ":" ss1
    let ss2' = splitOn ":" <$> ss1'
    ss3 <- case ss2' of
        Nothing -> if Prelude.length ss2 == 8 then pure ss2 else Nothing
        Just ss2'' -> do
            let l = Prelude.length ss2 + Prelude.length ss2''
            if l < 8 then pure $ ss2 ++ Prelude.replicate (8 - l) "" ++ ss2'' else Nothing
    ss4 <- l8ListToTuple <$> foldNothing (readHex' <$> ss3)
    pure . tupleToHostAddress6 $ ss4
    where
    readHex' :: Text -> Maybe Word16
    readHex' t = case readHex . unpack $ t of
        [] -> Just 0
        [(res, "")] -> Just res
        _ -> Nothing


data Record = Record {
    address :: Text,
    pubkey :: Text,
    name :: Maybe Text
} deriving (Show, Generic)

instance Eq Record where
    (==) :: Record -> Record -> Bool
    (==) p q = pubkey p == pubkey q

instance Ord Record where
    compare :: Record -> Record -> Ordering
    compare p q = compare (pubkey p) (pubkey q)

data NS6Record = NS6Record {
    ns6Address :: HostAddress6,
    ns6Name :: Text
} deriving (Show, Generic)

instance Eq NS6Record where
    (==) :: NS6Record -> NS6Record -> Bool
    (==) p q = ns6Address p == ns6Address q

instance Ord NS6Record where
    compare :: NS6Record -> NS6Record -> Ordering
    compare p q = compare (ns6Address p) (ns6Address q)

recordToNS6Record :: Record -> Maybe NS6Record
recordToNS6Record r = case (addr6FromText . address $ r, name r) of
    (Just a, Just n) -> pure $ NS6Record a n 
    _ -> Nothing

data State = State {
    names :: TMVar (Set NS6Record), nT :: TMVar Int64,
    tree :: TMVar (Set HostAddress6), tT :: TMVar Int64,
    -- Sessions got block after replying n times
    sesExpir :: TMVar (Set Int32), sT :: TMVar Int64
}

