{-# LANGUAGE ScopedTypeVariables #-}

module Web.VKHS.Curl (vk_curl) where

import Prelude hiding (catch)
import Control.Exception (catch,bracket)
import Control.Concurrent (threadDelay)
import Control.Monad.Writer
import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import qualified Data.ByteString.UTF8 as U
import Network.Curlhs.Core

import Web.VKHS.Types

import qualified Data.ByteString.Char8 as BS

-- | Generic request sender. Uses delay to prevent application from being
-- blocked for flooding
vk_curl :: Env a -> Writer [CURLoption] () -> IO (Either String String)
vk_curl e w = do
    let askE x = return (x e)
    v <- askE verbose
    a <- askE useragent
    d <- askE delay_ms
    do
        buff <- newIORef BS.empty
        bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
            let 
                memwrite n = atomicModifyIORef buff
                    (\o -> (BS.append o n, CURL_WRITEFUNC_OK))
            in
            curl_easy_setopt curl $
                [ CURLOPT_HEADER         True
                , CURLOPT_WRITEFUNCTION (Just $ memwrite)
                , CURLOPT_SSL_VERIFYPEER False
                , CURLOPT_USERAGENT $ BS.pack a
                , CURLOPT_VERBOSE (v == Debug )
                ] ++ (execWriter w);
            curl_easy_perform curl;
            threadDelay (1000 * d); -- delay in microseconds
            b <- readIORef buff ;
            return (Right $ U.toString b) ;
        } `catch`
            (\(e::CURLcode) -> return $ Left ("CURL error: " ++ (show e)))

