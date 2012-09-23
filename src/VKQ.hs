{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Label.Abstract
import Data.Typeable
import Data.Data
import Network.Protocol.Uri.Query
import Options.Applicative
import System.Exit
import System.Environment
import System.IO
import Text.Printf
import Text.JSON
import Text.JSON.Types
import Text.JSON.Generic
import Web.VKHS
import qualified Web.VKHS.API.JSON as J

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

cmd (Options v (Music mo@(MO act l))) = do
  let e = (envcall act) { verbose = v }
  ea <- J.api e "audio.get" []
  mc <- (checkRight >=> fromJS) ea
  processMC mo mc

cmd (Options v (UserQ uo@(UO act qs))) = do
  let e = (envcall act) { verbose = v }
  print qs
  ea <- J.api e "users.search" [("q",qs),("fields","uid,first_name,last_name,photo,education")]
  ur <- (checkRight >=> fromJS) ea
  processUQ uo ur

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

processMC :: MusicOptions -> Collection MusicRecord -> IO ()
processMC (MO _ True) (MC r) = do
  forM_ r $ \m -> do
    printf "%d|%s|%s|%s\n" (aid m) (artist m) (title m) (url m)

processMC (MO _ False) (MC r) = do
  print r

data UserRecord = UR
  { uid :: Int
  , first_name :: String
  , last_name :: String
  , photo :: String
  , university :: Maybe String
  } deriving(Show,Data,Typeable)

processUQ :: UserOptions -> Collection UserRecord -> IO ()
processUQ (UO _ _) (MC ur) = do
  print ur

