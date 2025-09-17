{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MLCloudHTTP where

import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=), (.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.HTTP.Req
import System.Directory (doesFileExist)

-- | Base URL of the remote cloud server, using HTTPS
-- Replace this with your actual cloud server URL
cloudServerBase :: Url 'Https
cloudServerBase = https "your.cloud.server.com"

--------------------------------------------------------------------------------
-- Data Types for JSON communication

-- | Payload for uploading ID and week number
data UploadPayload = UploadPayload
  { week :: Int          -- ^ The week number for which the ID applies
  , idCode :: Text       -- ^ The 16-hex ID code string
  } deriving (Show, Generic)

instance ToJSON UploadPayload where
  -- Custom JSON key names
  toJSON (UploadPayload w idc) = object
    [ "week" .= w
    , "id"   .= idc
    ]

-- | Response structure expected when fetching peers
data PeersResponse = PeersResponse
  { peers :: [Text]  -- ^ List of peer IDs for the given week
  } deriving (Show, Generic)

instance FromJSON PeersResponse where
  -- Expects JSON: { "peers": ["id1", "id2", ...] }
  parseJSON = Aeson.withObject "PeersResponse" $ \v ->
    PeersResponse <$> v .: "peers"

--------------------------------------------------------------------------------
-- File storage for fallback/local cache

-- | Path of the local ID file used as fallback or cache
localIDFile :: FilePath
localIDFile = "local_id.txt"

-- | Write the current ID to a local file
writeLocalID :: Text -> IO ()
writeLocalID idCode = TIO.writeFile localIDFile idCode

-- | Read the local ID from file (if exists)
readLocalID :: IO (Maybe Text)
readLocalID = do
  exists <- doesFileExist localIDFile
  if exists then Just <$> TIO.readFile localIDFile else return Nothing

--------------------------------------------------------------------------------
-- Upload the current ID and week number to the remote server

uploadID :: Text -> Int -> IO (Either String ())
uploadID idCode weekNum = do
  let payload = UploadPayload weekNum idCode
  -- Run the HTTP POST request using `req`
  result <- try $ runReq defaultHttpConfig $ do
    req POST
      (cloudServerBase /: "uploadID")
      (ReqBodyJson payload)
      ignoreResponse
      mempty
  case result of
    Left (e :: HttpException) ->
      return $ Left ("HTTP Exception: " ++ show e)
    Right _ -> return $ Right ()

--------------------------------------------------------------------------------
-- Fetch peer IDs from the remote server for a given week

getPeers :: Int -> IO (Either String [Text])
getPeers weekNum = do
  let params = "week" =: weekNum
  result <- try $ runReq defaultHttpConfig $ do
    r <- req GET
      (cloudServerBase /: "getPeers")
      NoReqBody
      jsonResponse
      params
    return (responseBody r :: PeersResponse)
  case result of
    Left (e :: HttpException) ->
      return $ Left ("HTTP Exception: " ++ show e)
    Right (PeersResponse peerList) -> return $ Right peerList

--------------------------------------------------------------------------------
-- | Fallback mechanism:
-- Try uploadID, if it fails fallback to writing local file instead
uploadIDWithFallback :: Text -> Int -> IO ()
uploadIDWithFallback idCode weekNum = do
  res <- uploadID idCode weekNum
  case res of
    Right () -> putStrLn "Upload succeeded."
    Left err -> do
      putStrLn ("Upload failed: " ++ err)
      putStrLn "Saving ID locally instead."
      writeLocalID idCode

-- | Fallback for getting peers:
-- Try remote fetch, if it fails, return empty list or local cached peers (you can customize)
getPeersWithFallback :: Int -> IO [Text]
getPeersWithFallback weekNum = do
  res <- getPeers weekNum
  case res of
    Right peersList -> return peersList
    Left err -> do
      putStrLn ("Failed to fetch peers: " ++ err)
      -- You can extend this to load from local cache if you want
      return []

--------------------------------------------------------------------------------
-- | Example usage:

{-
main :: IO ()
main = do
  let currentID = "0123456789ABCDEF" -- Example 16-hex ID string
      currentWeek = 37               -- Example week number

  -- Upload ID with fallback
  uploadIDWithFallback (pack currentID) currentWeek

  -- Get peers with fallback
  peersList <- getPeersWithFallback currentWeek
  putStrLn $ "Peers for week " ++ show currentWeek ++ ": " ++ show peersList
-}

--------------------------------------------------------------------------------
-- Notes:
-- 1) You need to have a remote server that supports /uploadID and /getPeers endpoints.
--    Those endpoints must accept and return JSON in the described formats.
--
-- 2) The current code falls back to local file storage only for upload failures.
--    You can extend this fallback logic as needed.
--
-- 3) To build and run, add `req` and `aeson` to your dependencies.
--
-- 4) Always secure your communication with HTTPS and validate SSL certificates.