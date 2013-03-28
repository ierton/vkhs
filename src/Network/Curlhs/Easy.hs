-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Easy
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Easy
  ( curl_version
  , curl_version_info
  , curl_easy_strerror
  , curl_easy_init
  , curl_easy_reset
  , curl_easy_cleanup
  , curl_easy_perform
  , curl_easy_getinfo
  , curl_easy_setopt
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable      (peek, sizeOf)
import Foreign.C.String      (peekCString)
import Foreign.C.Types       (CChar, CInt)
import Foreign.Ptr           (Ptr, nullPtr, plusPtr)

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock       (UTCTime)
import Data.Maybe            (mapMaybe)
import Data.Bits             ((.&.))
import Data.IORef            (newIORef)

import Control.Applicative   ((<$>), (<*>))
import Control.Exception     (throwIO)

import Network.Curlhs.Errors
import Network.Curlhs.Setopt
import Network.Curlhs.Types
import Network.Curlhs.Base


-------------------------------------------------------------------------------
-- | Returns the libcurl version string
--   (<http://curl.haxx.se/libcurl/c/curl_version.html>).
-------------------------------------------------------------------------------
curl_version :: IO String
curl_version = ccurl_version >>= peekCString


-------------------------------------------------------------------------------
-- | Returns run-time libcurl version info
--   (<http://curl.haxx.se/libcurl/c/curl_version_info.html>).
-------------------------------------------------------------------------------
curl_version_info :: IO CURL_version_info_data
curl_version_info = ccurl_version_info cCURLVERSION_NOW >>= peek >>=
  \cval -> CURL_version_info_data
    <$> (peekCString      $ ccurl_version_info_data_version         cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_version_num     cval)
    <*> (peekCString      $ ccurl_version_info_data_host            cval)
    <*> (peekCFeatures    $ ccurl_version_info_data_features        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ssl_version     cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ssl_version_num cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libz_version    cval)
    <*> (peekCStringList  $ ccurl_version_info_data_protocols       cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_ares            cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_ares_num        cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libidn          cval)
    <*> (peekCIntegral    $ ccurl_version_info_data_iconv_ver_num   cval)
    <*> (peekCStringMaybe $ ccurl_version_info_data_libssh_version  cval)

peekCStringList :: Ptr (Ptr CChar) -> IO [String]
peekCStringList ptr = peek ptr >>= \cstring ->
  if (cstring == nullPtr) then return [] else do
    let size = sizeOf (undefined :: Ptr CChar)
    strings <- peekCStringList (plusPtr ptr size)
    string  <- peekCString cstring
    return (string : strings)

peekCStringMaybe :: Ptr CChar -> IO (Maybe String)
peekCStringMaybe ptr = if (ptr /= nullPtr)
  then Just <$> peekCString ptr
  else return Nothing

peekCIntegral :: (Num h, Integral c) => c -> IO h
peekCIntegral = return . fromIntegral

peekCFeatures :: CInt -> IO [CURL_version]
peekCFeatures mask =
  return $ mapMaybe (\(v, b) -> if (mask .&. b == 0) then Nothing else Just v)
    [ (CURL_VERSION_IPV6        , cCURL_VERSION_IPV6        )
    , (CURL_VERSION_KERBEROS4   , cCURL_VERSION_KERBEROS4   )
    , (CURL_VERSION_SSL         , cCURL_VERSION_SSL         )
    , (CURL_VERSION_LIBZ        , cCURL_VERSION_LIBZ        )
    , (CURL_VERSION_NTLM        , cCURL_VERSION_NTLM        )
    , (CURL_VERSION_GSSNEGOTIATE, cCURL_VERSION_GSSNEGOTIATE)
    , (CURL_VERSION_DEBUG       , cCURL_VERSION_DEBUG       )
    , (CURL_VERSION_ASYNCHDNS   , cCURL_VERSION_ASYNCHDNS   )
    , (CURL_VERSION_SPNEGO      , cCURL_VERSION_SPNEGO      )
    , (CURL_VERSION_LARGEFILE   , cCURL_VERSION_LARGEFILE   )
    , (CURL_VERSION_IDN         , cCURL_VERSION_IDN         )
    , (CURL_VERSION_SSPI        , cCURL_VERSION_SSPI        )
    , (CURL_VERSION_CONV        , cCURL_VERSION_CONV        )
    , (CURL_VERSION_CURLDEBUG   , cCURL_VERSION_CURLDEBUG   )
    , (CURL_VERSION_TLSAUTH_SRP , cCURL_VERSION_TLSAUTH_SRP ) |7214:----|
    , (CURL_VERSION_NTLM_WB     , cCURL_VERSION_NTLM_WB     ) |7220:----|
    ]


-------------------------------------------------------------------------------
-- | Start a libcurl easy session
--   (<http://curl.haxx.se/libcurl/c/curl_easy_init.html>).
-------------------------------------------------------------------------------
curl_easy_init :: IO CURL
curl_easy_init = ccurl_easy_init >>= \ccurl -> if (ccurl == nullPtr)
  then throwIO CURLE_FAILED_INIT
  else CURL ccurl
    <$> newIORef Nothing
    <*> newIORef Nothing


-------------------------------------------------------------------------------
-- | Reset all options of a libcurl session handle
--   (<http://curl.haxx.se/libcurl/c/curl_easy_reset.html>).
-------------------------------------------------------------------------------
curl_easy_reset :: CURL -> IO ()
curl_easy_reset curl =
  ccurl_easy_reset (ccurlptr curl) >> freeCallbacks curl


-------------------------------------------------------------------------------
-- | End a libcurl easy session
--   (<http://curl.haxx.se/libcurl/c/curl_easy_cleanup.html>).
-------------------------------------------------------------------------------
curl_easy_cleanup :: CURL -> IO ()
curl_easy_cleanup curl =
  ccurl_easy_cleanup (ccurlptr curl) >> freeCallbacks curl


-------------------------------------------------------------------------------
-- | Perform a file transfer
--   (<http://curl.haxx.se/libcurl/c/curl_easy_perform.html>).
-------------------------------------------------------------------------------
curl_easy_perform :: CURL -> IO ()
curl_easy_perform curl = withCODE $ ccurl_easy_perform (ccurlptr curl)


-------------------------------------------------------------------------------
-- | Extract information from a curl handle
--   (<http://curl.haxx.se/libcurl/c/curl_easy_getinfo.html>).
-------------------------------------------------------------------------------
curl_easy_getinfo :: CURL -> IO CURLinfo
curl_easy_getinfo curl = let ccurl = ccurlptr curl in CURLinfo
  <$> getinfo'String   ccurl cCURLINFO_EFFECTIVE_URL
  <*> getinfo'RespCode ccurl cCURLINFO_RESPONSE_CODE
  <*> getinfo'RespCode ccurl cCURLINFO_HTTP_CONNECTCODE
  <*> getinfo'FileTime ccurl cCURLINFO_FILETIME
  <*> getinfo'Double   ccurl cCURLINFO_TOTAL_TIME
  <*> getinfo'Double   ccurl cCURLINFO_NAMELOOKUP_TIME
  <*> getinfo'Double   ccurl cCURLINFO_CONNECT_TIME
  <*> getinfo'Double   ccurl cCURLINFO_APPCONNECT_TIME
  <*> getinfo'Double   ccurl cCURLINFO_PRETRANSFER_TIME
  <*> getinfo'Double   ccurl cCURLINFO_STARTTRANSFER_TIME
  <*> getinfo'Double   ccurl cCURLINFO_REDIRECT_TIME
  <*> getinfo'Int      ccurl cCURLINFO_REDIRECT_COUNT
  <*> getinfo'MString  ccurl cCURLINFO_REDIRECT_URL
  <*> getinfo'Double   ccurl cCURLINFO_SIZE_UPLOAD
  <*> getinfo'Double   ccurl cCURLINFO_SIZE_DOWNLOAD
  <*> getinfo'Double   ccurl cCURLINFO_SPEED_DOWNLOAD
  <*> getinfo'Double   ccurl cCURLINFO_SPEED_UPLOAD
  <*> getinfo'Int      ccurl cCURLINFO_HEADER_SIZE
  <*> getinfo'Int      ccurl cCURLINFO_REQUEST_SIZE
  <*> getinfo'Int      ccurl cCURLINFO_SSL_VERIFYRESULT
  <*> getinfo'SList    ccurl cCURLINFO_SSL_ENGINES
  <*> getinfo'ContentL ccurl cCURLINFO_CONTENT_LENGTH_DOWNLOAD
  <*> getinfo'ContentL ccurl cCURLINFO_CONTENT_LENGTH_UPLOAD
  <*> getinfo'MString  ccurl cCURLINFO_CONTENT_TYPE
--  <*> getinfo'String   ccurl cCURLINFO_PRIVATE
  <*> getinfo'CurlAuth ccurl cCURLINFO_HTTPAUTH_AVAIL
  <*> getinfo'CurlAuth ccurl cCURLINFO_PROXYAUTH_AVAIL
  <*> getinfo'Int      ccurl cCURLINFO_OS_ERRNO
  <*> getinfo'Int      ccurl cCURLINFO_NUM_CONNECTS
  <*> getinfo'String   ccurl cCURLINFO_PRIMARY_IP
  <*> getinfo'Int      ccurl cCURLINFO_PRIMARY_PORT
  <*> getinfo'String   ccurl cCURLINFO_LOCAL_IP
  <*> getinfo'Int      ccurl cCURLINFO_LOCAL_PORT
  <*> getinfo'SList    ccurl cCURLINFO_COOKIELIST
  <*> getinfo'Socket   ccurl cCURLINFO_LASTSOCKET
  <*> getinfo'MString  ccurl cCURLINFO_FTP_ENTRY_PATH
  <*> getinfo'CertInfo ccurl cCURLINFO_CERTINFO
  <*> getinfo'TimeCond ccurl cCURLINFO_CONDITION_UNMET
  <*> getinfo'MString  ccurl cCURLINFO_RTSP_SESSION_ID
  <*> getinfo'Int      ccurl cCURLINFO_RTSP_CLIENT_CSEQ
  <*> getinfo'Int      ccurl cCURLINFO_RTSP_SERVER_CSEQ
  <*> getinfo'Int      ccurl cCURLINFO_RTSP_CSEQ_RECV

getinfo'String :: Ptr CCURL -> CCURLinfo'CString -> IO String
getinfo'String ccurl cinfo = alloca $ \ptr -> do
  withCODE $ ccurl_easy_getinfo'CString ccurl cinfo ptr
  peek ptr >>= peekCString

getinfo'MString :: Ptr CCURL -> CCURLinfo'CString -> IO (Maybe String)
getinfo'MString ccurl cinfo = alloca $ \ptr -> do
  withCODE $ ccurl_easy_getinfo'CString ccurl cinfo ptr
  peek ptr >>= \csptr -> if (csptr /= nullPtr)
    then Just <$> peekCString csptr
    else return Nothing

getinfo'Double :: Ptr CCURL -> CCURLinfo'CDouble -> IO Double
getinfo'Double ccurl cinfo = alloca $ \ptr -> do
  withCODE $ ccurl_easy_getinfo'CDouble ccurl cinfo ptr
  realToFrac <$> peek ptr

getinfo'ContentL :: Ptr CCURL -> CCURLinfo'CDouble -> IO (Maybe Double)
getinfo'ContentL ccurl cinfo = getinfo'Double ccurl cinfo >>= \v ->
  return $ if (v == (-1)) then Nothing else Just v

getinfo'SList :: Ptr CCURL -> CCURLinfo'SList -> IO [String]
getinfo'SList ccurl cinfo = alloca $ \ptr -> do
  withCODE $ ccurl_easy_getinfo'SList ccurl cinfo ptr
  peek ptr >>= \slist -> do
    strings <- peek'CCURL_slist slist
    ccurl_slist_free_all slist
    return strings

getinfo'CertInfo :: Ptr CCURL -> CCURLinfo'CertI -> IO [[String]]
getinfo'CertInfo ccurl cinfo = alloca $ \ptr -> do
  withCODE $ ccurl_easy_getinfo'CertI ccurl cinfo ptr
  peek ptr >>= peek'CCURL_certinfo

getinfo'Int :: Ptr CCURL -> CCURLinfo'CLong -> IO Int
getinfo'Int ccurl cinfo = alloca $ \ptr -> do
  withCODE $ ccurl_easy_getinfo'CLong ccurl cinfo ptr
  fromIntegral <$> peek ptr

getinfo'RespCode :: Ptr CCURL -> CCURLinfo'CLong -> IO (Maybe Int)
getinfo'RespCode ccurl cinfo = getinfo'Int ccurl cinfo >>= \v ->
  return $ if (v == 0) then Nothing else Just v

getinfo'FileTime :: Ptr CCURL -> CCURLinfo'CLong -> IO (Maybe UTCTime)
getinfo'FileTime ccurl cinfo = getinfo'Int ccurl cinfo >>= \v ->
  return $ if (v == (-1) || v == 0) then Nothing
    else Just (posixSecondsToUTCTime $ realToFrac v)

getinfo'Socket :: Ptr CCURL -> CCURLinfo'CLong -> IO (Maybe Int)
getinfo'Socket ccurl cinfo = getinfo'Int ccurl cinfo >>= \v ->
  return $ if (v == (-1)) then Nothing else Just v

getinfo'TimeCond :: Ptr CCURL -> CCURLinfo'CLong -> IO Bool
getinfo'TimeCond ccurl cinfo = toBool <$> getinfo'Int ccurl cinfo

getinfo'CurlAuth :: Ptr CCURL -> CCURLinfo'CLong -> IO [CURLauth]
getinfo'CurlAuth ccurl cinfo = do
  mask <- fromIntegral <$> getinfo'Int ccurl cinfo
  return $ mapMaybe (\(v, b) -> if (mask .&. b == 0) then Nothing else Just v)
    [ (CURLAUTH_BASIC       , cCURLAUTH_BASIC       )
    , (CURLAUTH_DIGEST      , cCURLAUTH_DIGEST      )
    , (CURLAUTH_DIGEST_IE   , cCURLAUTH_DIGEST_IE   )
    , (CURLAUTH_GSSNEGOTIATE, cCURLAUTH_GSSNEGOTIATE)
    , (CURLAUTH_NTLM        , cCURLAUTH_NTLM        )
    , (CURLAUTH_NTLM_WB     , cCURLAUTH_NTLM_WB     ) |7220:----|
    ]


peek'CCURL_slist :: Ptr CCURL_slist -> IO [String]
peek'CCURL_slist ptr =
  if (ptr == nullPtr) then return [] else peek ptr >>= \slist -> do
    slist_head <- peekCString      $ ccurl_slist_data slist
    slist_tail <- peek'CCURL_slist $ ccurl_slist_next slist
    return (slist_head : slist_tail)

peek'CCURL_certinfo :: Ptr CCURL_certinfo -> IO [[String]]
peek'CCURL_certinfo ptr =
  if (ptr == nullPtr) then return [] else peek ptr >>= \certinfo -> do
    let numOfCerts = fromIntegral $ ccurl_certinfo_num_of_certs certinfo
    let size = sizeOf (undefined :: Ptr CCURL_slist)
    let ptr0 = ccurl_certinfo_certinfo certinfo
    let ptrs = map (\i -> plusPtr ptr0 (i * size)) [0 .. (numOfCerts - 1)]
    mapM (\sptr -> peek sptr >>= peek'CCURL_slist) ptrs

