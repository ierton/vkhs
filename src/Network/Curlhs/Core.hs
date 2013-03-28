-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Core
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
--
-- Module "Network.Curlhs.Core" provides a mid-level interface to @libcurl@.
-- For a direct low-level bindings go to "Network.Curlhs.Base".
--
-- API of this module follows the API of @libcurl@ as defined in version
-- 7.25.0 of the library. But it also depends on the version of @libcurl@
-- that is used during compilation of the @curlhs@ package. It is possible
-- to use @curlhs@ with older versions of @libcurl@, just keep in mind
-- that some features may not be available then.
--
-- There is not much documentation here, maybe the future will change that,
-- but for now please use the original @libcurl@ documentation. API provided
-- here follows the original API, so this shouldn't be a big problem.
-- Documentation about @libcurl@ and/or its particular functions may be
-- found in manual pages, which are available among others at the @libcurl@
-- project site (please refer to <http://curl.haxx.se/libcurl/>).
-- 
-- Exposed API is still somewhat incomplete, but is usable.
-- Work on the rest are in progress.
--
-- Simple example:
--
-- > import qualified Data.ByteString.Char8 as BS
-- > import Data.IORef (newIORef, readIORef, atomicModifyIORef)
-- > import Control.Exception (bracket)
-- > import Network.Curlhs.Core
-- > 
-- > curlGET :: BS.ByteString -> IO BS.ByteString
-- > curlGET url = do
-- >   buff <- newIORef BS.empty
-- >   bracket (curl_easy_init) (curl_easy_cleanup) $ \curl -> do
-- >     curl_easy_setopt curl
-- >       [ CURLOPT_URL     url
-- >       , CURLOPT_VERBOSE True
-- >       , CURLOPT_WRITEFUNCTION $ Just (memwrite buff)
-- >       ]
-- >     curl_easy_perform curl
-- >   readIORef buff
-- > 
-- > memwrite buff newbs = atomicModifyIORef buff $ \oldbuff ->
-- >   (BS.append oldbuff newbs, CURL_WRITEFUNC_OK)
--
-------------------------------------------------------------------------------

module Network.Curlhs.Core (

  -- * Global interface

  -- ** Version info
    curl_version
  , curl_version_info
  , CURL_version_info_data (..)
  , CURL_version (..)

  -- ** Error codes
  -- |  More about error codes in libcurl on
  --    <http://curl.haxx.se/libcurl/c/libcurl-errors.html>
  , curl_easy_strerror
  , CURLcode (..)

  -- * Easy interface
  -- | See <http://curl.haxx.se/libcurl/c/libcurl-easy.html>
  --   for easy interface overview.

  -- ** Init, reset, cleanup
  , curl_easy_init
  , curl_easy_reset
  , curl_easy_cleanup
  , CURL

  -- ** Transfer
  , curl_easy_perform

  -- ** Get info
  , curl_easy_getinfo
  , CURLinfo (..)

  -- ** Set options
  , curl_easy_setopt
  , CURLoption (..)

  -- *** Callbacks
  , CURL_write_callback, CURL_write_response (..)
  , CURL_read_callback , CURL_read_response  (..)

  -- *** Constants
  , CURLproto     (..)
  , CURLproxy     (..)
  , CURLnetrc     (..)
  , CURLauth      (..)
  , CURLtlsauth   (..) |7214:----|
  , CURLredir     (..)
  , CURLhttpver   (..)
  , CURLftpcreate (..)
  , CURLftpauth   (..)
  , CURLftpssl    (..)
  , CURLftpmethod (..)
  , CURLrtspreq   (..)
  , CURLtimecond  (..)
  , CURLclosepol  (..)
  , CURLipresolve (..)
  , CURLusessl    (..)
  , CURLsslver    (..)
  , CURLsslopt    (..) |7250:----|
  , CURLgssapi    (..) |7220:----|
  , CURLsshauth   (..)

  ) where

import Network.Curlhs.Types
import Network.Curlhs.Easy

