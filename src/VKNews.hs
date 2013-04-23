
module Main where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Either
import Data.Time.Clock
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.RegexPR
import Web.VKHS as VK hiding (api,api')
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


data Pirozhok = Pirozhok
  { plines :: String
  , pdate :: UTCTime
  }

pprint p = liftIO $ do
  -- putStrLn (show $ pdate p)
  putStr (plines p)

type PState a = StateT UTCTime (VKAPI IO) a

pirozhok wr@(WR _ _ _ t d) = Pirozhok <$> poetry <*> date
  where
    poetry = txt >>= noempty >>= flt >>= Just . unlines
    txt = pure $ lines $ gsubRegexPR "<br>" "\n" $ takeWhile (/= '©') t
    noempty ls = pure $ filter (/=[]) ls
    flt ls | length ls >4 = Nothing
           | otherwise = pure ls
    date = pure (publishedAt wr)

new_enough d (Pirozhok _ d') = d' > d

vkhs_app_id = "3128877"

env_var_name = "VKNEWS_ACCESS_TOKEN"

gid_piro = "-28122932"

opts at = Options
  <$> flag Normal Debug (long "verbose" <> help "Be verbose")
  <*> strOption (long "application-id" <> short 'a' <> value vkhs_app_id <> help (printf "Application ID (can be set via %s)" env_var_name))
  <*> strOption (long "access-token" <> short 't' <> value at <> help "Access token")
  <*> option (long "poll-interval" <> short 'i' <> value 20 <> help "Poll interval [sec]")
  <*> argument str (metavar "USERNAME" <> help "User name")
  <*> strOption (metavar "STR" <> long "password" <> short 'p' <> value "-" <> help "Password")
  <*> argument str (metavar "GROUPID" <> help "Vkontakte ID of the group to read the news from")

cmd :: Options -> IO ()
cmd (Options v aid at pollint u p gid) = run $ do
  forever $ do
    d <- getTime
    Response (SL len ws) <- callVK "wall.get" [("owner_id",gid_piro)]
    forM (to_pirozhok d ws) $ \p@(Pirozhok text d') -> do
        pprint p
        pmsg []
        putMaxTime d'
    sleep_sec pollint

    where

      to_pirozhok d ws = filter (new_enough d) $ catMaybes $ map pirozhok ws
      -- to_pirozhok d ws = catMaybes $ map pirozhok ws

      run vk = do
        t <- getCurrentTime
        let e = (VK.env aid u p VK.allAccess) { verbose = v }
        let ma = runStateT vk t
        r <- runVKAPI ma ([],[],[]) e
        case r of
          Left er -> do
            perror (show er)
            exitFailure
          Right ((a,_),_) -> return a

      perror s = liftIO $ hPutStrLn stderr s

      pmsg s = liftIO $ putStrLn s

      callVK a b = lift $ api' a b

      putMaxTime d' = get >>= \d -> if d' > d then put d' else return ()

      getTime = get

      sleep_sec s = liftIO $ threadDelay (1000 * 1000 * s)

main = do
  at <- fromMaybe [] <$> lookupEnv env_var_name
  execParser (info (opts at) idm) >>= cmd

