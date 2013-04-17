
module Main where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Either
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Web.VKHS as VK
import Web.VKHS.API.Aeson as VK
import Web.VKHS.API.Monad as VK

data Options = Options
  { verb :: Verbosity
  , application_id :: String
  , access_token :: String
  , vk_poll_interval_sec :: Int
  , username :: String
  , password :: String
  , vk_group_id :: String
  } deriving(Show)

vkhs_app_id = "3128877"

env_var_name = "VKNEWS_ACCESS_TOKEN"

gid_piro = "-28122932"

opts at = Options
  <$> flag Normal Debug (long "verbose" <> help "Be verbose")
  <*> strOption (long "application-id" <> short 'a' <> value vkhs_app_id <> help (printf "Application ID (can be set via %s)" env_var_name))
  <*> strOption (long "access-token" <> short 't' <> value at <> help "Access token")
  <*> option (long "poll-interval" <> short 'i' <> value 3 <> help "Poll interval [sec]")
  <*> argument str (metavar "USERNAME" <> help "User name")
  <*> strOption (metavar "STR" <> long "password" <> short 'p' <> value "-" <> help "Password")
  <*> argument str (metavar "GROUPID" <> help "Vkontakte ID of the group to read the news from")

cmd :: Options -> IO ()
cmd (Options v aid at pollint u p gid) = run $ do
  forever $ do
    Response (SL len ws) <- apiM' "wall.get" [("owner_id",gid_piro)]
    liftIO $ putStrLn (show $ head (ws :: [WallRecord]))
    liftIO $ threadDelay (1000 * 1000 * pollint); -- convert sec to us

    where

      run vk = do
        let e = (VK.env aid u p VK.allAccess) { verbose = v }
        r <- runVKAPI vk ([],[],[]) e
        case r of
          Left er -> do
            hPutStrLn stderr (show er)
            exitFailure
          Right (a,_) -> return a

main = do
  at <- fromMaybe [] <$> lookupEnv env_var_name
  execParser (info (opts at) idm) >>= cmd

-- algo = do
--   forever $ do
--     f <- fetch
--     extract f
--     sleep

