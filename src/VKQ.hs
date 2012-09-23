module Main where

import Control.Monad
import Data.Label.Abstract
import Network.Protocol.Uri.Query
import Options.Applicative
import System.Exit
import System.IO
import Text.Printf
import Web.VKHS

data Options = Options
  { verb :: Verbosity
  , cmdOpts :: CmdOptions
  } deriving(Show)

data CmdOptions
  = Login LoginOptions
  | Call CallOptions
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

loginOptions :: Parser CmdOptions
loginOptions = Login <$> (LoginOptions
  <$> argument str (metavar "APPID" & help "Application identifier (provided to you by vk.com)")
  <*> argument str (metavar "USER" & help "User name or email")
  <*> argument str (metavar "PASS" & help "User password"))

callOptions :: Parser CmdOptions
callOptions = Call <$> (CallOptions
  <$> argument str (metavar "ACCESS_TOKEN" & help "access_token")
  <*> argument str (metavar "METHOD" & help "Method to call")
  <*> argument str (metavar "PARAMS" & help "Method arguments, KEY=VALUE[,KEY2=VALUE2[,,,]]"))

opts :: Parser Options
opts = Options
  <$> flag Normal Debug (long "verbose" & help "Be verbose")
  <*> subparser (
    command "login" (info loginOptions
        ( progDesc "Login into vk.com (see --help for details); Print access_token (among user_id and expiration time) on success" ))
    & command "call" (info callOptions
        ( progDesc "call VK API method (see --help for details)" ))
    )

ifeither e fl fr = either fl fr e

errexit e = hPutStrLn stderr e >> exitFailure

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

main :: IO ()
main = execParser (info opts idm) >>= cmd

