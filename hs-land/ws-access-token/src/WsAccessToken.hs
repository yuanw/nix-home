{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WsAccessToken where

import Control.Applicative (liftA2, liftA3)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (String),
    withObject,
    (.:),
  )
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time.Clock.POSIX
  ( POSIXTime,
    getPOSIXTime,
  )
import Data.Vector qualified as V
import Dhall
  ( FromDhall,
    Generic,
    Text,
    Vector,
    auto,
    input,
  )
import Network.HTTP.Req
  ( POST (POST),
    ReqBodyUrlEnc (ReqBodyUrlEnc),
    Scheme (Https),
    Url,
    defaultHttpConfig,
    https,
    jsonResponse,
    renderUrl,
    req,
    responseBody,
    runReq,
    (/:),
    (=:),
  )
import System.Environment (getArgs, lookupEnv)
import Web.JWT
  ( ClaimsMap (ClaimsMap),
    EncodeSigner (EncodeRSAPrivateKey),
    JWTClaimsSet
      ( aud,
        exp,
        iss,
        sub,
        unregisteredClaims
      ),
    StringOrURI,
    claims,
    decode,
    encodeSigned,
    numericDate,
    readRsaSecret,
    stringOrURI,
  )
import Prelude hiding (exp)

data Record = Record
  { keyPath :: Text,
    issuer :: Text,
    scopes :: Vector Text,
    membershipId :: Maybe Text,
    audience :: Text
  }
  deriving (Generic, Show)

instance FromDhall Record

newtype AccessToken = AccessToken
  { value :: String
  }

instance FromJSON AccessToken where
  parseJSON = withObject "AccessToken" $ \obj -> do
    val <- obj .: "access_token"
    return (AccessToken val)

getSigner :: Record -> IO EncodeSigner
getSigner config = do
  content <- (BS.readFile . T.unpack . keyPath) config
  maybe
    (fail "fail to read secret key")
    (return . EncodeRSAPrivateKey)
    (readRsaSecret content)

constructClaimsSet :: Record -> POSIXTime -> JWTClaimsSet
constructClaimsSet config posix =
  mempty -- mempty returns a default JWTClaimsSet
    { iss = stringOrURI (issuer config),
      sub = (stringOrURI <=< membershipId) config,
      exp = numericDate posix,
      aud =
        Left
          <$> stringOrURI
            ((renderUrl . https . audience) config),
      unregisteredClaims =
        ClaimsMap $
          Map.fromList
            [("scope", (String . T.intercalate " " . V.toList . scopes) config)]
    }

-- sign :: Record -> IO T.Text
-- sign config = do
--   signer <- getSigner config
--   posix  <- getPOSIXTime
--   return $ encodeSigned signer mempty (constructClaimsSet config posix)

sign :: Record -> IO T.Text
sign config = liftA3 encodeSigned (getSigner config) (return mempty) (constructClaimsSet config <$> getPOSIXTime)

prepareRequest :: Text -> Url 'Https
prepareRequest aud = foldl (/:) (https host) tails
  where
    parts = T.splitOn "/" aud
    host = head parts
    tails = tail parts

send :: T.Text -> Record -> IO AccessToken
send assertion config = runReq defaultHttpConfig $ do
  let payload =
        "grant_type"
          =: ("urn:ietf:params:oauth:grant-type:jwt-bearer" :: T.Text)
          <> "assertion"
            =: assertion

  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (prepareRequest (audience config))
      -- (https "wk-dev.wdesk.org" /: "iam" /: "oauth2" /: "v4.0" /: "token") -- safe by construction URL
      (ReqBodyUrlEnc payload) -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.
  liftIO $ return (responseBody r :: AccessToken)

getToken :: Record -> IO AccessToken
getToken record = do
  assertion <- sign record
  send assertion record

getAccessToken :: IO ()
getAccessToken = do
  args <- getArgs
  record <- loadRecord (head args)
  token <- getToken record
  (putStrLn . value) token

getConfigDir :: IO String
getConfigDir = liftA2 (++) base (pure "ws-access-token/")
  where
    base =
      fmap
        (maybe "$HOME/." (++ "/"))
        (lookupEnv "XDG_CONFIG_HOME")

loadRecord :: String -> IO Record
loadRecord name = do
  file <- fmap (++ (name ++ ".dhall")) getConfigDir
  input auto (T.pack file)

debugToken :: T.Text -> Maybe JWTClaimsSet
debugToken = fmap claims . decode
