{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Label.Abstract
import Data.Typeable
import Data.Data
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import Network.Protocol.Uri.Query
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Environment
import System.IO
import Text.Printf
import Text.JSON
import Text.JSON.Types
import Text.JSON.Generic
import Text.PFormat (pformat)
import Text.Namefilter (namefilter)
import Web.VKHS
import Web.VKHS.Curl
import qualified Web.VKHS.API.Aeson as A
import qualified Web.VKHS.API.JSON as J
import qualified Data.Aeson as A

data Options = Options
  { verb :: Verbosity
  , cmdOpts :: CmdOptions
  } deriving(Show)

data CmdOptions
  = Login LoginOptions
  | Call CallOptions
  | Music MusicOptions
  | UserQ UserOptions
  deriving(Show)

data LoginOptions = LoginOptions
  { appid :: String
  , username :: String
  , password :: String
  } deriving(Show)

data CallOptions = CO
  { accessToken :: String
  , method :: String
  , args :: String
  } deriving(Show)

data MusicOptions = MO
  { accessToken_m :: String
  , list_music :: Bool
  , name_format :: String
  , output_format :: String
  , out_dir :: String
  , records_id :: [String]
  } deriving(Show)

data UserOptions = UO
  { accessToken_u :: String
  , queryString :: String
  } deriving(Show)

loginOptions :: Parser CmdOptions
loginOptions = Login <$> (LoginOptions
  <$> argument str (metavar "APPID" & help "Application identifier (provided to you by vk.com)")
  <*> argument str (metavar "USER" & help "User name or email")
  <*> argument str (metavar "PASS" & help "User password"))

-- opts :: String -> Parser Options
opts m =
  let access_token_flag = strOption (short 'a' & m & metavar "ACCESS_TOKEN" &
        help "Access token. Honores VKQ_ACCESS_TOKEN environment variable")
  in Options
  <$> flag Normal Debug (long "verbose" & help "Be verbose")
  <*> subparser (
    command "login" (info loginOptions
      ( progDesc "Login and print access token (also prints user_id and expiriration time)" ))
    & command "call" (info (Call <$> (CO
      <$> access_token_flag
      <*> argument str (metavar "METHOD" & help "Method name")
      <*> argument str (metavar "PARAMS" & help "Method arguments, KEY=VALUE[,KEY2=VALUE2[,,,]]")))
      ( progDesc "Call VK API method" ))
    & command "music" (info ( Music <$> (MO
      <$> access_token_flag
      <*> switch (long "list" & short 'l' & help "List music files")
      <*> strOption (metavar "FORMAT" & short 'f' & value "id %o_%i author %a title %t url %u" & help "Listing format, supported tags: %i %o %a %t %d %u" )
      <*> strOption (metavar "FORMAT" & short 'F' & value "%a - %t" & help "FileName format, supported tags: %i %o %a %t %d %u" )
      <*> strOption (metavar "DIR" & short 'o' & help "Output directory" & value "")
      <*> arguments str (metavar "RECORD_ID" & help "Download records")
      ))
      ( progDesc "List or download music files"))
    & command "user" (info ( UserQ <$> (UO
      <$> access_token_flag
      <*> strOption (long "query" & short 'q' & help "String to query")
      ))
      ( progDesc "Extract various user information"))
    )

ifeither e fl fr = either fl fr e

errexit e = hPutStrLn stderr e >> exitFailure

checkRight (Left e) = hPutStrLn stderr e >> exitFailure
checkRight (Right a) = return a

fromJS :: (Data a) => JSValue -> IO a
fromJS jv = checkR . fromJSON $ jv where
  checkR (Ok a) = return a
  checkR (Error s) = checkRight (Left (s ++ " JSValue: " ++ (show jv)))

main :: IO ()
main = do
  m <- maybe (idm) (value) <$> lookupEnv "VKQ_ACCESS_TOKEN"
  execParser (info (opts m) idm) >>= cmd

cmd :: Options -> IO ()
cmd (Options v (Login (LoginOptions a u p))) = do
  let e = (env a u p allAccess) { verbose = v }
  ea <- login e
  ifeither ea errexit $ \(at,uid,expin) -> do
    printf "%s %s %s\n" at uid expin

cmd (Options v (Call (CO act mn args))) = do
  let e = (envcall act) { verbose = v }
  ea <- api e mn (fw (keyValues "," "=") args)
  ifeither ea errexit putStrLn

-- list audio files summary
cmd (Options v (Music mo@(MO act True fmt _ _ _))) = do
  let e = (envcall act) { verbose = v }
  ea <- J.api e "audio.get" []
  MC mc <- (checkRight >=> fromJS) ea
  forM_ mc $ \m -> do
    printf "%s\n" (mr_format fmt m)

cmd (Options v (Music mo@(MO act False _ ofmt odir rid))) = do
  let e = (envcall act) { verbose = v }
  ea <- J.api e "audio.getById" [("audios", concat $ intersperse "," rid)]
  (MC mc) <- (checkRight >=> fromJS) ea
  forM_ mc $ \m -> do
    (fp, h) <- openFileMR odir ofmt m
    r <- vk_curl_file e (url m) $ \ bs -> do
      BS.hPut h bs
    checkRight r
    printf "%d_%d\n" (owner_id m) (aid m)
    printf "%s\n" (title m)
    printf "%s\n" fp

cmd (Options v (UserQ uo@(UO act qs))) = do
  let e = (envcall act) { verbose = v }
  print qs
  ea <- J.api e "users.search" [("q",qs),("fields","uid,first_name,last_name,photo,education")]
  ae <- checkRight ea
  processUQ uo ae

type NameFormat = String

openFileMR :: FilePath -> NameFormat -> MusicRecord -> IO (FilePath, Handle)
openFileMR [] _ m = do
  let (_,ext) = splitExtension (url m)
  temp <- getTemporaryDirectory
  (fp,h) <- openBinaryTempFile temp ("vkqmusic"++ext)
  return (fp,h)
openFileMR dir fmt m = do
  let (_,ext) = splitExtension (url m)
  let name = mr_format fmt m
  let name' = replaceExtension name ext
  let fp =  (dir </> name') 
  h <- openBinaryFile fp WriteMode
  return (fp,h)

data Collection a = MC {
  response :: [a]
  } deriving (Show,Data,Typeable)

data MusicRecord = MR
  { aid :: Int
  , owner_id :: Int
  , artist :: String
  , title :: String
  , duration :: Int
  , url :: String
  } deriving (Show,Data,Typeable)


mr_format :: String -> MusicRecord -> String
mr_format s mr = pformat '%'
  [ ('i', show . aid)
  , ('o', show . owner_id)
  , ('a', namefilter . artist)
  , ('t', namefilter . title)
  , ('d', show . duration)
  , ('u', url)
  ] s mr

-- processMC :: MusicOptions -> Collection MusicRecord -> IO ()
-- processMC (MO _ _ _) (MC r) = do
--   forM_ r $ \m -> do
--     printf "%d_%d|%s|%s|%s\n" (owner_id m) (aid m) (artist m) (title m) (url m)

-- processMC (MO _ rid False) (MC r) = do
--   print r

data UserRecord = UR
  { uid :: Int
  , first_name :: String
  , last_name :: String
  , photo :: String
  , university :: Maybe Int
  , university_name :: Maybe String
  , faculty :: Maybe Int
  , faculty_name :: Maybe String
  , graduation :: Maybe Int
  } deriving(Show,Data,Typeable)

parseUsers :: JSValue -> Maybe [JSValue]
parseUsers (JSObject (JSONObject [("response",(JSArray a))])) = Just a
parseUsers _ = Nothing

processUQ :: UserOptions -> JSValue -> IO ()
processUQ (UO _ _) j = do
  -- let (Just u) = parseUsers j
  -- a <- fromJS (u !! 1)
  -- print $ show (a :: UserRecord)
  print $ show $ j


