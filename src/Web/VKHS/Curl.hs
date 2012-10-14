{-# LANGUAGE ScopedTypeVariables #-}

module Web.VKHS.Curl
  ( vk_curl
  , vk_curl_file
  ) where

import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as BS

import Control.Applicative
import Control.Exception (catch,bracket)
import Control.Concurrent (threadDelay)
import Control.Monad.Writer

import Network.Curlhs.Core

import Prelude hiding (catch)

import System.IO

import Web.VKHS.Types

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
            threadDelay (1000 * d); -- convert ms to us
            b <- readIORef buff ;
            return (Right $ U.toString b) ;
        } `catch`
            (\(e::CURLcode) -> return $ Left ("CURL error: " ++ (show e)))


scanPattern pat s =
  let (_,o,x,n) = BS.foldl' check (False, BS.empty, BS.empty, BS.empty) s
  in (BS.reverse o, x, BS.reverse n)
  where 
    check (False, old, st, new) b
      | BS.length st < BS.length pat = (False, old, st`BS.snoc`b, new)
      | st == pat                    = (True, old, st, b`BS.cons`new) 
      | otherwise                    = (False, (BS.head st)`BS.cons`old, (BS.tail st)`BS.snoc`b, new)
    check (True, old, st, new) b     = (True, old, st, b`BS.cons`new)

cutheader :: BS.ByteString -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
cutheader buf new =
  let
  buf' = BS.append buf new
  overfill = BS.length buf' >= 1024
  (_,p,t) = scanPattern (BS.pack "\r\n\r\n") buf'
  in case (overfill, BS.null t) of
    (True, True) -> Nothing
    (False, True) -> Just (buf', t)
    (_, False) -> Just (buf', t)

data State = Pending BS.ByteString | Working BS.ByteString | FailNoHeader

process :: State -> BS.ByteString -> State
process s@(Pending b) bs =
  case cutheader b bs of
    Nothing -> FailNoHeader
    Just (b', t)
      | BS.null t -> (Pending b')
      | otherwise -> (Working t)
process s@(Working _) bs = (Working bs)
process s _ = s

-- | Downloads the file and saves it on disk
vk_curl_file
  :: Env a
  -- ^ User environment
  -> String
  -- ^ URL
  -> (BS.ByteString -> IO ())
  -- ^ File write handler
  -> IO (Either String ())
vk_curl_file e url cb = do
    let askE x = return (x e)
    v <- askE verbose
    a <- askE useragent
    d <- askE delay_ms
    do
        sr <- newIORef =<< (Pending <$> pure BS.empty)

        bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
            let 
            filewrite bs = do
              s' <- atomicModifyIORef sr (\s -> let s' = process s bs in (s',s'))
              case s' of
                FailNoHeader -> return CURL_WRITEFUNC_FAIL
                Pending b -> return CURL_WRITEFUNC_OK
                Working t -> do
                  cb t
                  return CURL_WRITEFUNC_OK 
            in
              curl_easy_setopt curl $
                [ CURLOPT_HEADER         True
                , CURLOPT_WRITEFUNCTION (Just $ filewrite)
                , CURLOPT_SSL_VERIFYPEER False
                , CURLOPT_USERAGENT $ BS.pack a
                , CURLOPT_VERBOSE (v == Debug)
                , CURLOPT_URL (BS.pack url)
                ]; 

            curl_easy_perform curl;
            threadDelay (1000 * d); -- convert ms to us
            s <- readIORef sr;
            case s of
              Working _ -> return $ Right ()
              _         -> return $ Left "HTTP header detection failure"
        } `catch`
            (\(e::CURLcode) -> return $ Left ("CURL error: " ++ (show e)))


