module Web.VKHS.Api where

import Control.Monad.Writer

import Data.Label
import qualified Data.ByteString.Char8 as BS

import Network.Curlhs.Core

import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Protocol.Uri.Query

import Text.Printf

import Web.VKHS.Types
import Web.VKHS.Curl

api :: Env -> AccessToken -> String -> Parameters -> IO (Either String String)
api e (at,_,_) mn mp =
    let uri = BS.pack $ showUri $ (\f -> f $ toUri $ printf "https://api.vk.com/method/%s" mn) $
                set query $ bw params (("access_token",at):mp)
    in vk_curl e $ do
        tell [CURLOPT_URL  uri]

