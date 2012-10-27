{-# LANGUAGE ScopedTypeVariables #-}

module Test.Curl where

import Web.VKHS
import Text.Printf

import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import Data.ByteString.Char8 as BS

import Control.Applicative
import Control.Exception (catch,bracket)
import Control.Concurrent (threadDelay)
import Control.Monad.Writer
-- import qualified Data.ByteString.UTF8 as U
import Network.Curlhs.Core
import System.IO

import Prelude hiding (catch)

scanPattern pat s =
  let (_,o,x,n) = BS.foldl' check (False, BS.empty, BS.empty, BS.empty) s
  in (BS.reverse o, x, BS.reverse n)
  where 
    check (False, old, st, new) b
      | BS.length st < BS.length pat = (False, old, st`BS.snoc`b, new)
      | st == pat                    = (True, old, st, b`BS.cons`new) 
      | otherwise                    = (False, (BS.head st)`cons`old, (BS.tail st)`snoc`b, new)
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

data State = Pending Handle BS.ByteString | Working Handle BS.ByteString | FailNoHeader

process :: State -> BS.ByteString -> State
process s@(Pending h b) bs =
  case cutheader b bs of
    Nothing -> FailNoHeader
    Just (b', t)
      | BS.null t -> (Pending h b')
      | otherwise -> (Working h t)
process s@(Working h _) bs = (Working h bs)
process s _ = s

vk_curl_download :: Env a -> FilePath -> String -> IO (Either String ())
vk_curl_download e f url = do
    let askE x = return (x e)
    v <- askE verbose
    a <- askE useragent
    d <- askE delay_ms
    do
        sr <- newIORef =<< (Pending <$> openBinaryFile f WriteMode <*> pure BS.empty)

        bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do {
            let 
            filewrite bs = do
              s' <- atomicModifyIORef sr (\s -> let s' = process s bs in (s',s'))
              case s' of
                FailNoHeader -> return CURL_WRITEFUNC_FAIL
                Pending h b -> return CURL_WRITEFUNC_OK
                Working h t -> do
                  BS.hPut h t
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
              Working _ _ -> return $ Right ()
              _           -> return $ Left "HTTP header detection failure"
        } `catch`
            (\(e::CURLcode) -> return $ Left ("CURL error: " ++ (show e)))


download = do
  undefined

