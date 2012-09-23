{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Label.Abstract
import Data.Typeable
import Data.Data
import Network.Protocol.Uri.Query
import Options.Applicative
import System.Exit
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
  deriving(Show)

data LoginOptions = LoginOptions
  { appid :: String
  , username :: String
  , password :: String
  } deriving(Show)

data CallOptions = CallOptions
  { accessToken :: String
  , method :: String
  , args :: String
  } deriving(Show)

data MusicOptions = MO
  { list_music :: Bool
  , accessToken_m :: String
  } deriving(Show)

loginOptions :: Parser CmdOptions
loginOptions = Login <$> (LoginOptions
  <$> argument str (metavar "APPID" & help "Application identifier (provided to you by vk.com)")
  <*> argument str (metavar "USER" & help "User name or email")
  <*> argument str (metavar "PASS" & help "User password"))

callOptions :: Parser CmdOptions
callOptions = Call <$> (CallOptions
  <$> argument str (metavar "ACCESS_TOKEN" & help "Access token")
  <*> argument str (metavar "METHOD" & help "Method name")
  <*> argument str (metavar "PARAMS" & help "Method arguments, KEY=VALUE[,KEY2=VALUE2[,,,]]"))

opts :: Parser Options
opts = Options
  <$> flag Normal Debug (long "verbose" & help "Be verbose")
  <*> subparser (
    command "login" (info loginOptions
      ( progDesc "Login and print access token (also prints user_id and expiriration time)" ))
    & command "call" (info callOptions
      ( progDesc "Call VK API method" ))
    & command "music" (info ( Music <$> (MO
      <$> switch (long "list" & short 'l' & help "List music files")
      <*> argument str (metavar "ACCESS_TOKEN" & help "Access token")
      ))
      ( progDesc "List or download music files"))
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
main = execParser (info opts idm) >>= cmd

cmd :: Options -> IO ()
cmd (Options v (Login (LoginOptions a u p))) = do
  let e = (env a u p allAccess) { verbose = v }
  ea <- login e
  ifeither ea errexit $ \(at,uid,expin) -> do
    printf "%s %s %s\n" at uid expin

cmd (Options v (Call (CallOptions act mn args))) = do
  let e = (envcall act) { verbose = v }
  ea <- api e mn (fw (keyValues "," "=") args)
  ifeither ea errexit putStrLn

cmd (Options v (Music mo@(MO l act))) = do
  let e = (envcall act) { verbose = v }
  ea <- J.api e "audio.get" []
  mc <- (checkRight >=> fromJS) ea
  processMC mo mc

data MusicCollection = MC {
  response :: [MusicRecord]
  } deriving (Show,Data,Typeable)

data MusicRecord = MR
  { aid :: Int
  , owner_id :: Int
  , artist :: String
  , title :: String
  , duration :: Int
  , url :: String
  } deriving (Show,Data,Typeable)

processMC :: MusicOptions -> MusicCollection -> IO ()
processMC (MO True _) (MC r) = do
  forM_ r $ \m -> do
    printf "%d|%s|%s|%s\n" (aid m) (artist m) (title m) (url m)

processMC (MO False _) (MC r) = do
  print r


