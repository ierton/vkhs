-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Base
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
--
-- Module "Network.Curlhs.Base" provides a direct low-level bindings to
-- @libcurl@. It is basically a 1:1 mapping of the @libcurl@'s C API,
-- a direct translation of \"curl/curl.h\" header files to Haskell FFI.
-- A higher level interface, without ubiquitous pointers and all of that
-- C stuff, is provided through the module "Network.Curlhs.Core".
--
-- Documentation about the library and/or particular functions may be found
-- in the @libcurl@'s manual pages or on the @libcurl@'s project site
-- (<http://curl.haxx.se/libcurl/>). Because API of this module mirrors API
-- of the external library, particular symbols may exist or not,
-- dependently of that, which version of @libcurl@ is used during compilation
-- of the package. The module as closely as possible tries to follow
-- the original @libcurl@ API. The main differences are in types of functions
-- such as @curl_easy_setopt@ and @curl_easy_getinfo@. Besides that all
-- symbol names are prefixed with \'c\' or \'C\'. 
--
-- As the name of the module may suggest, this module is a basis for the
-- rest of @curlhs@ package. For now exposed API is somewhat incomplete,
-- still lacks some things (like the \"multi interface\"), but the aim is
-- to provide here a complete API of @libcurl@, as defined in its C headers.
--
-------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}

module Network.Curlhs.Base where

import Foreign.C.Types
import Foreign.Storable (Storable (..))
import Foreign.Ptr      (Ptr, FunPtr, castPtr)

import Control.Applicative ((<$>), (<*>))


-------------------------------------------------------------------------------
#{let alignof type = "(%ld)", (long) offsetof (struct {char x; type y;}, y)}

#define CURL_NO_OLDIES
#include "curl/curl.h"

#define hsc_symbol(name, type) \
  printf("c" #name " :: C" #type "\n"); \
  printf("c" #name " =  C" #type " "); hsc_const(name);

#define hsc_cconst(name, type) \
  printf("c" #name " :: " #type "\n"); \
  printf("c" #name " =  "); hsc_const(name);


-------------------------------------------------------------------------------
-- * Definitions from \"curl/curlver.h\"
-------------------------------------------------------------------------------
libCURL_COPYRIGHT :: String
libCURL_COPYRIGHT = #{const_str LIBCURL_COPYRIGHT}

libCURL_TIMESTAMP :: String
libCURL_TIMESTAMP = #{const_str LIBCURL_TIMESTAMP}

libCURL_VERSION   :: String
libCURL_VERSION   = #{const_str LIBCURL_VERSION  }

libCURL_VERSION_NUM   :: Int
libCURL_VERSION_NUM   = #{const LIBCURL_VERSION_NUM  }

libCURL_VERSION_MAJOR :: Int
libCURL_VERSION_MAJOR = #{const LIBCURL_VERSION_MAJOR}

libCURL_VERSION_MINOR :: Int
libCURL_VERSION_MINOR = #{const LIBCURL_VERSION_MINOR}

libCURL_VERSION_PATCH :: Int
libCURL_VERSION_PATCH = #{const LIBCURL_VERSION_PATCH}


-------------------------------------------------------------------------------
-- * Definitions from \"curl/curlbuild.h\"
-------------------------------------------------------------------------------
type CCURL_off_t = CLLong  -- ??


-------------------------------------------------------------------------------
-- * Definitions from \"curl/curlrules.h\"
-------------------------------------------------------------------------------
-- todo


-------------------------------------------------------------------------------
-- * Definitions from \"curl/curl.h\"
-------------------------------------------------------------------------------
data CCURL


-------------------------------------------------------------------------------
#if defined(WIN32) && !defined(__LWIP_OPT_H__)
type CCURL_socket_t = CUIntPtr -- SOCKET??
#else
type CCURL_socket_t = CInt
#endif

#{cconst CURL_SOCKET_BAD, CCURL_socket_t}


-------------------------------------------------------------------------------
-- ** CURL_httppost
-------------------------------------------------------------------------------
data CCURL_httppost = CCURL_httppost
  { ccurl_httppost_next           :: Ptr CCURL_httppost
  , ccurl_httppost_name           :: Ptr CChar
  , ccurl_httppost_namelength     :: CLong
  , ccurl_httppost_contents       :: Ptr CChar
  , ccurl_httppost_contentslength :: CLong
  , ccurl_httppost_buffer         :: Ptr CChar
  , ccurl_httppost_bufferlength   :: CLong
  , ccurl_httppost_contenttype    :: Ptr CChar
  , ccurl_httppost_contentheader  :: Ptr CCURL_slist
  , ccurl_httppost_more           :: Ptr CCURL_httppost
  , ccurl_httppost_flags          :: CLong
  , ccurl_httppost_showfilename   :: Ptr CChar
  , ccurl_httppost_userp          :: Ptr ()
  } deriving (Show)

-- instance Storable CCURL_httppost where
--   sizeOf _    = #{size    struct curl_httppost}
--   alignment _ = #{alignof struct curl_httppost}
--   poke _ _    = undefined
--   peek _      = undefined

#{cconst HTTPPOST_FILENAME   , CLong}
#{cconst HTTPPOST_READFILE   , CLong}
#{cconst HTTPPOST_PTRNAME    , CLong}
#{cconst HTTPPOST_PTRCONTENTS, CLong}
#{cconst HTTPPOST_BUFFER     , CLong}
#{cconst HTTPPOST_PTRBUFFER  , CLong}
#{cconst HTTPPOST_CALLBACK   , CLong}
 
-------------------------------------------------------------------------------
-- ** Callbacks
-------------------------------------------------------------------------------
-- *** CURL_progress_callback
-------------------------------------------------------------------------------
type CCURL_progress_callback
  = Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> IO CInt

foreign import ccall "wrapper"
  wrap_ccurl_progress_callback
    :: CCURL_progress_callback
    -> IO (FunPtr CCURL_progress_callback)


-------------------------------------------------------------------------------
-- *** CURL_write_callback
-------------------------------------------------------------------------------
#{cconst CURL_MAX_WRITE_SIZE , CSize}
#{cconst CURL_MAX_HTTP_HEADER, CSize}
#{cconst CURL_WRITEFUNC_PAUSE, CSize}

type CCURL_write_callback
  = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
  wrap_ccurl_write_callback
    :: CCURL_write_callback
    -> IO (FunPtr CCURL_write_callback)


-------------------------------------------------------------------------------
-- ** CURL_fileinfo
-------------------------------------------------------------------------------
newtype CCURLfiletype = CCURLfiletype CInt deriving (Eq, Show)   |7210:----|

#{symbol CURLFILETYPE_FILE        , CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_DIRECTORY   , CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_SYMLINK     , CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_DEVICE_BLOCK, CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_DEVICE_CHAR , CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_NAMEDPIPE   , CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_SOCKET      , CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_DOOR        , CURLfiletype}                |7210:----|
#{symbol CURLFILETYPE_UNKNOWN     , CURLfiletype}                |7210:----|

#{cconst CURLFINFOFLAG_KNOWN_FILENAME  , CUInt}                  |7210:----|
#{cconst CURLFINFOFLAG_KNOWN_FILETYPE  , CUInt}                  |7210:----|
#{cconst CURLFINFOFLAG_KNOWN_TIME      , CUInt}                  |7210:----|
#{cconst CURLFINFOFLAG_KNOWN_PERM      , CUInt}                  |7210:----|
#{cconst CURLFINFOFLAG_KNOWN_UID       , CUInt}                  |7210:----|
#{cconst CURLFINFOFLAG_KNOWN_GID       , CUInt}                  |7210:----|
#{cconst CURLFINFOFLAG_KNOWN_SIZE      , CUInt}                  |7210:----|
#{cconst CURLFINFOFLAG_KNOWN_HLINKCOUNT, CUInt}                  |7210:----|

data CCURL_fileinfo = CCURL_fileinfo                             |7210:----|
  { ccurl_fileinfo_filename       :: Ptr CChar                   |7210:----|
  , ccurl_fileinfo_filetype       :: CCURLfiletype               |7210:----|
  , ccurl_fileinfo_time           :: CTime                       |7210:----|
  , ccurl_fileinfo_perm           :: CUInt                       |7210:----|
  , ccurl_fileinfo_uid            :: CInt                        |7210:----|
  , ccurl_fileinfo_gid            :: CInt                        |7210:----|
  , ccurl_fileinfo_size           :: CCURL_off_t                 |7210:----|
  , ccurl_fileinfo_hardlinks      :: CLong                       |7210:----|
  , ccurl_fileinfo_strings_time   :: Ptr CChar                   |7210:----|
  , ccurl_fileinfo_strings_perm   :: Ptr CChar                   |7210:----|
  , ccurl_fileinfo_strings_user   :: Ptr CChar                   |7210:----|
  , ccurl_fileinfo_strings_group  :: Ptr CChar                   |7210:----|
  , ccurl_fileinfo_strings_target :: Ptr CChar                   |7210:----|
  , ccurl_fileinfo_flags          :: CUInt                       |7210:----|
  , ccurl_fileinfo_b_data         :: Ptr CChar                   |7210:----|
  , ccurl_fileinfo_b_size         :: CSize                       |7210:----|
  , ccurl_fileinfo_b_used         :: CSize                       |7210:----|
  } deriving (Show)                                              |7210:----|

-- instance Storable CCURL_fileinfo where                        |7210:----|
--   sizeOf _    = #{size    struct curl_fileinfo}               |7210:----|
--   alignment _ = #{alignof struct curl_fileinfo}               |7210:----|
--   poke _ _    = undefined                                     |7210:----|
--   peek _      = undefined                                     |7210:----|

 
-------------------------------------------------------------------------------
-- ** Callbacks
-------------------------------------------------------------------------------
-- *** CURL_chunk_bgn_callback
-------------------------------------------------------------------------------
#{cconst CURL_CHUNK_BGN_FUNC_OK  , CLong}                        |7210:----|
#{cconst CURL_CHUNK_BGN_FUNC_FAIL, CLong}                        |7210:----|
#{cconst CURL_CHUNK_BGN_FUNC_SKIP, CLong}                        |7210:----|

type CCURL_chunk_bgn_callback                                    |7210:----|
  = Ptr () -> Ptr () -> CInt -> IO CLong                         |7210:----|

foreign import ccall "wrapper"                                   |7210:----|
  wrap_ccurl_chunk_bgn_callback                                  |7210:----|
    :: CCURL_chunk_bgn_callback                                  |7210:----|
    -> IO (FunPtr CCURL_chunk_bgn_callback)                      |7210:----|


-------------------------------------------------------------------------------
-- *** CURL_chunk_end_callback
-------------------------------------------------------------------------------
#{cconst CURL_CHUNK_END_FUNC_OK  , CLong}                        |7210:----|
#{cconst CURL_CHUNK_END_FUNC_FAIL, CLong}                        |7210:----|

type CCURL_chunk_end_callback                                    |7210:----|
  = Ptr () -> IO CLong                                           |7210:----|

foreign import ccall "wrapper"                                   |7210:----|
  wrap_ccurl_chunk_end_callback                                  |7210:----|
    :: CCURL_chunk_end_callback                                  |7210:----|
    -> IO (FunPtr CCURL_chunk_end_callback)                      |7210:----|

 
-------------------------------------------------------------------------------
-- *** CURL_fnmatch_callback
-------------------------------------------------------------------------------
#{cconst CURL_FNMATCHFUNC_MATCH  , CInt}                         |7210:----|
#{cconst CURL_FNMATCHFUNC_NOMATCH, CInt}                         |7210:----|
#{cconst CURL_FNMATCHFUNC_FAIL   , CInt}                         |7210:----|

type CCURL_fnmatch_callback                                      |7210:----|
  = Ptr () -> Ptr CChar -> Ptr CChar -> IO CInt                  |7210:----|

foreign import ccall "wrapper"                                   |7210:----|
  wrap_ccurl_fnmatch_callback                                    |7210:----|
    :: CCURL_fnmatch_callback                                    |7210:----|
    -> IO (FunPtr CCURL_fnmatch_callback)                        |7210:----|

 
-------------------------------------------------------------------------------
-- *** CURL_seek_callback
-------------------------------------------------------------------------------
#{cconst CURL_SEEKFUNC_OK      , CInt}
#{cconst CURL_SEEKFUNC_FAIL    , CInt}
#{cconst CURL_SEEKFUNC_CANTSEEK, CInt}

type CCURL_seek_callback
  = Ptr () -> CCURL_off_t -> CInt -> IO CInt

foreign import ccall "wrapper"
  wrap_ccurl_seek_callback
    :: CCURL_seek_callback
    -> IO (FunPtr CCURL_seek_callback)


-------------------------------------------------------------------------------
-- *** CURL_read_callback
-------------------------------------------------------------------------------
#{cconst CURL_READFUNC_ABORT, CSize}
#{cconst CURL_READFUNC_PAUSE, CSize}

type CCURL_read_callback
  = Ptr CChar -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
  wrap_ccurl_read_callback
    :: CCURL_read_callback
    -> IO (FunPtr CCURL_read_callback)


-------------------------------------------------------------------------------
-- *** CURL_sockopt_callback
-------------------------------------------------------------------------------
newtype CCURLsocktype = CCURLsocktype CInt deriving (Eq, Show)

#{symbol CURLSOCKTYPE_IPCXN, CURLsocktype}

#{cconst CURL_SOCKOPT_OK               , CInt}                   |7215:----|
#{cconst CURL_SOCKOPT_ERROR            , CInt}                   |7215:----|
#{cconst CURL_SOCKOPT_ALREADY_CONNECTED, CInt}                   |7215:----|

type CCURL_sockopt_callback
  = Ptr () -> CCURL_socket_t -> CCURLsocktype -> IO CInt

foreign import ccall "wrapper"
  wrap_ccurl_sockopt_callback
    :: CCURL_sockopt_callback
    -> IO (FunPtr CCURL_sockopt_callback)


-------------------------------------------------------------------------------
-- *** CURL_opensocket_callback
-------------------------------------------------------------------------------
data CCURL_sockaddr = CCURL_sockaddr
  { ccurl_sockaddr_family   :: CInt
  , ccurl_sockaddr_socktype :: CInt
  , ccurl_sockaddr_protocol :: CInt
  , ccurl_sockaddr_addrlen  :: CUInt
  , ccurl_sockaddr_addr     :: Ptr ()  -- sockaddr?? TODO
  } deriving (Show)

-- instance Storable CCURL_sockaddr where
--   sizeOf _    = #{size    struct curl_sockaddr}
--   alignment _ = #{alignof struct curl_sockaddr}
--   poke _ _    = undefined
--   peek _      = undefined

type CCURL_opensocket_callback
  = Ptr () -> CCURLsocktype -> Ptr CCURL_sockaddr -> IO CCURL_socket_t

foreign import ccall "wrapper"
  wrap_ccurl_opensocket_callback
    :: CCURL_opensocket_callback
    -> IO (FunPtr CCURL_opensocket_callback)


-------------------------------------------------------------------------------
-- *** CURL_closesocket_callback
-------------------------------------------------------------------------------
type CCURL_closesocket_callback                                  |7217:----|
  = Ptr () -> CCURL_socket_t -> IO CInt                          |7217:----|

foreign import ccall "wrapper"                                   |7217:----|
  wrap_ccurl_closesocket_callback                                |7217:----|
    :: CCURL_closesocket_callback                                |7217:----|
    -> IO (FunPtr CCURL_closesocket_callback)                    |7217:----|

 
-------------------------------------------------------------------------------
-- *** CURL_ioctl_callback
-------------------------------------------------------------------------------
newtype CCURLioerr = CCURLioerr CInt deriving (Eq, Show)

#{symbol CURLIOE_OK           , CURLioerr}
#{symbol CURLIOE_UNKNOWNCMD   , CURLioerr}
#{symbol CURLIOE_FAILRESTART  , CURLioerr}

newtype CCURLiocmd = CCURLiocmd CInt deriving (Eq, Show)

#{symbol CURLIOCMD_NOP        , CURLiocmd}
#{symbol CURLIOCMD_RESTARTREAD, CURLiocmd}

type CCURL_ioctl_callback
  = Ptr CCURL -> CCURLiocmd -> Ptr () -> IO CCURLioerr

foreign import ccall "wrapper"
  wrap_ccurl_ioctl_callback
    :: CCURL_ioctl_callback
    -> IO (FunPtr CCURL_ioctl_callback)


-------------------------------------------------------------------------------
-- *** CURL_malloc_callback
-------------------------------------------------------------------------------
type CCURL_malloc_callback
  = CSize -> IO (Ptr ())

foreign import ccall "wrapper"
  wrap_ccurl_malloc_callback
    :: CCURL_malloc_callback
    -> IO (FunPtr CCURL_malloc_callback)


-------------------------------------------------------------------------------
-- *** CURL_free_callback
-------------------------------------------------------------------------------
type CCURL_free_callback
  = Ptr () -> IO ()

foreign import ccall "wrapper"
  wrap_ccurl_free_callback
    :: CCURL_free_callback
    -> IO (FunPtr CCURL_free_callback)


-------------------------------------------------------------------------------
-- *** CURL_realloc_callback
-------------------------------------------------------------------------------
type CCURL_realloc_callback
  = Ptr () -> CSize -> IO (Ptr ())

foreign import ccall "wrapper"
  wrap_ccurl_realloc_callback
    :: CCURL_realloc_callback
    -> IO (FunPtr CCURL_realloc_callback)


-------------------------------------------------------------------------------
-- *** CURL_strdup_callback
-------------------------------------------------------------------------------
type CCURL_strdup_callback
  = Ptr CChar -> IO (Ptr CChar)

foreign import ccall "wrapper"
  wrap_ccurl_strdup_callback
    :: CCURL_strdup_callback
    -> IO (FunPtr CCURL_strdup_callback)


-------------------------------------------------------------------------------
-- *** CURL_calloc_callback
-------------------------------------------------------------------------------
type CCURL_calloc_callback
  = CSize -> CSize -> IO (Ptr ())

foreign import ccall "wrapper"
  wrap_ccurl_calloc_callback
    :: CCURL_calloc_callback
    -> IO (FunPtr CCURL_calloc_callback)

 
-------------------------------------------------------------------------------
-- *** CURL_debug_callback
-------------------------------------------------------------------------------
newtype CCURL_infotype = CCURL_infotype CInt deriving (Eq, Show)

#{symbol CURLINFO_TEXT        , CURL_infotype}
#{symbol CURLINFO_HEADER_IN   , CURL_infotype}
#{symbol CURLINFO_HEADER_OUT  , CURL_infotype}
#{symbol CURLINFO_DATA_IN     , CURL_infotype}
#{symbol CURLINFO_DATA_OUT    , CURL_infotype}
#{symbol CURLINFO_SSL_DATA_IN , CURL_infotype}
#{symbol CURLINFO_SSL_DATA_OUT, CURL_infotype}
#{symbol CURLINFO_END         , CURL_infotype}

type CCURL_debug_callback
  = Ptr CCURL -> CCURL_infotype -> Ptr CChar -> CSize -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  wrap_ccurl_debug_callback
    :: CCURL_debug_callback
    -> IO (FunPtr CCURL_debug_callback)


-------------------------------------------------------------------------------
-- ** Constants
-------------------------------------------------------------------------------
-- *** CURLcode
-------------------------------------------------------------------------------
newtype CCURLcode = CCURLcode CInt deriving (Eq, Show)

#{symbol CURLE_OK                      , CURLcode}
#{symbol CURLE_UNSUPPORTED_PROTOCOL    , CURLcode}
#{symbol CURLE_FAILED_INIT             , CURLcode}
#{symbol CURLE_URL_MALFORMAT           , CURLcode}
#{symbol CURLE_NOT_BUILT_IN            , CURLcode} |7215:----|
#{symbol CURLE_COULDNT_RESOLVE_PROXY   , CURLcode}
#{symbol CURLE_COULDNT_RESOLVE_HOST    , CURLcode}
#{symbol CURLE_COULDNT_CONNECT         , CURLcode}
#{symbol CURLE_FTP_WEIRD_SERVER_REPLY  , CURLcode}
#{symbol CURLE_REMOTE_ACCESS_DENIED    , CURLcode}
#{symbol CURLE_FTP_ACCEPT_FAILED       , CURLcode} |7240:----|
#{symbol CURLE_FTP_WEIRD_PASS_REPLY    , CURLcode}
#{symbol CURLE_FTP_ACCEPT_TIMEOUT      , CURLcode} |7240:----|
#{symbol CURLE_FTP_WEIRD_PASV_REPLY    , CURLcode}
#{symbol CURLE_FTP_WEIRD_227_FORMAT    , CURLcode}
#{symbol CURLE_FTP_CANT_GET_HOST       , CURLcode}
#{symbol CURLE_FTP_COULDNT_SET_TYPE    , CURLcode}
#{symbol CURLE_PARTIAL_FILE            , CURLcode}
#{symbol CURLE_FTP_COULDNT_RETR_FILE   , CURLcode}
#{symbol CURLE_QUOTE_ERROR             , CURLcode}
#{symbol CURLE_HTTP_RETURNED_ERROR     , CURLcode}
#{symbol CURLE_WRITE_ERROR             , CURLcode}
#{symbol CURLE_UPLOAD_FAILED           , CURLcode}
#{symbol CURLE_READ_ERROR              , CURLcode}
#{symbol CURLE_OUT_OF_MEMORY           , CURLcode}
#{symbol CURLE_OPERATION_TIMEDOUT      , CURLcode}
#{symbol CURLE_FTP_PORT_FAILED         , CURLcode}
#{symbol CURLE_FTP_COULDNT_USE_REST    , CURLcode}
#{symbol CURLE_RANGE_ERROR             , CURLcode}
#{symbol CURLE_HTTP_POST_ERROR         , CURLcode}
#{symbol CURLE_SSL_CONNECT_ERROR       , CURLcode}
#{symbol CURLE_BAD_DOWNLOAD_RESUME     , CURLcode}
#{symbol CURLE_FILE_COULDNT_READ_FILE  , CURLcode}
#{symbol CURLE_LDAP_CANNOT_BIND        , CURLcode}
#{symbol CURLE_LDAP_SEARCH_FAILED      , CURLcode}
#{symbol CURLE_FUNCTION_NOT_FOUND      , CURLcode}
#{symbol CURLE_ABORTED_BY_CALLBACK     , CURLcode}
#{symbol CURLE_BAD_FUNCTION_ARGUMENT   , CURLcode}
#{symbol CURLE_INTERFACE_FAILED        , CURLcode}
#{symbol CURLE_TOO_MANY_REDIRECTS      , CURLcode}
#{symbol CURLE_UNKNOWN_TELNET_OPTION   , CURLcode} |----:7214|
#{symbol CURLE_UNKNOWN_OPTION          , CURLcode} |7215:----|
#{symbol CURLE_TELNET_OPTION_SYNTAX    , CURLcode}
#{symbol CURLE_PEER_FAILED_VERIFICATION, CURLcode}
#{symbol CURLE_GOT_NOTHING             , CURLcode}
#{symbol CURLE_SSL_ENGINE_NOTFOUND     , CURLcode}
#{symbol CURLE_SSL_ENGINE_SETFAILED    , CURLcode}
#{symbol CURLE_SEND_ERROR              , CURLcode}
#{symbol CURLE_RECV_ERROR              , CURLcode}
#{symbol CURLE_SSL_CERTPROBLEM         , CURLcode}
#{symbol CURLE_SSL_CIPHER              , CURLcode}
#{symbol CURLE_SSL_CACERT              , CURLcode}
#{symbol CURLE_BAD_CONTENT_ENCODING    , CURLcode}
#{symbol CURLE_LDAP_INVALID_URL        , CURLcode}
#{symbol CURLE_FILESIZE_EXCEEDED       , CURLcode}
#{symbol CURLE_USE_SSL_FAILED          , CURLcode}
#{symbol CURLE_SEND_FAIL_REWIND        , CURLcode}
#{symbol CURLE_SSL_ENGINE_INITFAILED   , CURLcode}
#{symbol CURLE_LOGIN_DENIED            , CURLcode}
#{symbol CURLE_TFTP_NOTFOUND           , CURLcode}
#{symbol CURLE_TFTP_PERM               , CURLcode}
#{symbol CURLE_REMOTE_DISK_FULL        , CURLcode}
#{symbol CURLE_TFTP_ILLEGAL            , CURLcode}
#{symbol CURLE_TFTP_UNKNOWNID          , CURLcode}
#{symbol CURLE_REMOTE_FILE_EXISTS      , CURLcode}
#{symbol CURLE_TFTP_NOSUCHUSER         , CURLcode}
#{symbol CURLE_CONV_FAILED             , CURLcode}
#{symbol CURLE_CONV_REQD               , CURLcode}
#{symbol CURLE_SSL_CACERT_BADFILE      , CURLcode}
#{symbol CURLE_REMOTE_FILE_NOT_FOUND   , CURLcode}
#{symbol CURLE_SSH                     , CURLcode}
#{symbol CURLE_SSL_SHUTDOWN_FAILED     , CURLcode}
#{symbol CURLE_AGAIN                   , CURLcode}
#{symbol CURLE_SSL_CRL_BADFILE         , CURLcode}
#{symbol CURLE_SSL_ISSUER_ERROR        , CURLcode}
#{symbol CURLE_FTP_PRET_FAILED         , CURLcode}
#{symbol CURLE_RTSP_CSEQ_ERROR         , CURLcode}
#{symbol CURLE_RTSP_SESSION_ERROR      , CURLcode}
#{symbol CURLE_FTP_BAD_FILE_LIST       , CURLcode} |7210:----|
#{symbol CURLE_CHUNK_FAILED            , CURLcode} |7210:----|


-------------------------------------------------------------------------------
-- ** Callbacks
-------------------------------------------------------------------------------
-- *** CURL_conv_callback
-------------------------------------------------------------------------------
type CCURL_conv_callback
  = Ptr CChar -> CSize -> IO CCURLcode

foreign import ccall "wrapper"
  wrap_ccurl_conv_callback
    :: CCURL_conv_callback
    -> IO (FunPtr CCURL_conv_callback)


-------------------------------------------------------------------------------
-- *** CURL_ssl_ctx_callback
-------------------------------------------------------------------------------
type CCURL_ssl_ctx_callback
  = Ptr CCURL -> Ptr () -> Ptr () -> IO CCURLcode

foreign import ccall "wrapper"
  wrap_ccurl_ssl_ctx_callback
    :: CCURL_ssl_ctx_callback
    -> IO (FunPtr CCURL_ssl_ctx_callback)

 
-------------------------------------------------------------------------------
-- ** Constants
-------------------------------------------------------------------------------
-- *** CURLproxy
-------------------------------------------------------------------------------
#{cconst CURLPROXY_HTTP           , CLong}
#{cconst CURLPROXY_HTTP_1_0       , CLong}
#{cconst CURLPROXY_SOCKS4         , CLong}
#{cconst CURLPROXY_SOCKS5         , CLong}
#{cconst CURLPROXY_SOCKS4A        , CLong}
#{cconst CURLPROXY_SOCKS5_HOSTNAME, CLong}


-------------------------------------------------------------------------------
-- *** CURLauth
-------------------------------------------------------------------------------
#{cconst CURLAUTH_NONE        , CLong}
#{cconst CURLAUTH_BASIC       , CLong}
#{cconst CURLAUTH_DIGEST      , CLong}
#{cconst CURLAUTH_GSSNEGOTIATE, CLong}
#{cconst CURLAUTH_NTLM        , CLong}
#{cconst CURLAUTH_DIGEST_IE   , CLong}
#{cconst CURLAUTH_NTLM_WB     , CLong} |7220:----|
#{cconst CURLAUTH_ONLY        , CLong} |7213:----|
#{cconst CURLAUTH_ANY         , CLong}
#{cconst CURLAUTH_ANYSAFE     , CLong}


-------------------------------------------------------------------------------
-- *** CURLsshauth
-------------------------------------------------------------------------------
#{cconst CURLSSH_AUTH_ANY      , CLong}
#{cconst CURLSSH_AUTH_NONE     , CLong}
#{cconst CURLSSH_AUTH_PUBLICKEY, CLong}
#{cconst CURLSSH_AUTH_PASSWORD , CLong}
#{cconst CURLSSH_AUTH_HOST     , CLong}
#{cconst CURLSSH_AUTH_KEYBOARD , CLong}
#{cconst CURLSSH_AUTH_DEFAULT  , CLong}


-------------------------------------------------------------------------------
-- *** CURLgssapi
-------------------------------------------------------------------------------
#{cconst CURLGSSAPI_DELEGATION_NONE       , CLong}               |7220:----|
#{cconst CURLGSSAPI_DELEGATION_POLICY_FLAG, CLong}               |7220:----|
#{cconst CURLGSSAPI_DELEGATION_FLAG       , CLong}               |7220:----|


-------------------------------------------------------------------------------
-- *** CURL_error_size
-------------------------------------------------------------------------------
#{cconst CURL_ERROR_SIZE, CLong}


-------------------------------------------------------------------------------
-- ** Callbacks
-------------------------------------------------------------------------------
-- *** CURL_sshkey_callback
-------------------------------------------------------------------------------
data CCURL_khkey = CCURL_khkey
  { ccurl_khkey_key     :: Ptr CChar
  , ccurl_khkey_len     :: CSize
  , ccurl_khkey_keytype :: CCURL_khtype
  } deriving (Show)

-- instance Storable CCURL_khkey where
--   sizeOf _    = #{size    struct curl_khkey}
--   alignment _ = #{alignof struct curl_khkey}
--   poke _ _    = undefined
--   peek _      = undefined

newtype CCURL_khtype = CCURL_khtype CInt deriving (Eq, Show)

#{symbol CURLKHTYPE_UNKNOWN, CURL_khtype}
#{symbol CURLKHTYPE_RSA1   , CURL_khtype}
#{symbol CURLKHTYPE_RSA    , CURL_khtype}
#{symbol CURLKHTYPE_DSS    , CURL_khtype}
 
newtype CCURL_khstat = CCURL_khstat CInt deriving (Eq, Show)

#{symbol CURLKHSTAT_FINE_ADD_TO_FILE, CURL_khstat}
#{symbol CURLKHSTAT_FINE            , CURL_khstat}
#{symbol CURLKHSTAT_REJECT          , CURL_khstat}
#{symbol CURLKHSTAT_DEFER           , CURL_khstat}

newtype CCURL_khmatch = CCURL_khmatch CInt deriving (Eq, Show)

#{symbol CURLKHMATCH_OK      , CURL_khmatch}
#{symbol CURLKHMATCH_MISMATCH, CURL_khmatch}
#{symbol CURLKHMATCH_MISSING , CURL_khmatch}
 
type CCURL_sshkey_callback
  = Ptr CCURL -> Ptr CCURL_khkey -> Ptr CCURL_khkey
    -> CCURL_khmatch -> Ptr () -> IO CCURL_khstat

foreign import ccall "wrapper"
  wrap_ccurl_sshkey_callback
    :: CCURL_sshkey_callback
    -> IO (FunPtr CCURL_sshkey_callback)

 
-------------------------------------------------------------------------------
-- ** Constants
-------------------------------------------------------------------------------
-- *** CURLusessl
-------------------------------------------------------------------------------
#{cconst CURLUSESSL_NONE   , CLong}
#{cconst CURLUSESSL_TRY    , CLong}
#{cconst CURLUSESSL_CONTROL, CLong}
#{cconst CURLUSESSL_ALL    , CLong}


-------------------------------------------------------------------------------
-- *** CURLsslopt
-------------------------------------------------------------------------------
#{cconst CURLSSLOPT_ALLOW_BEAST, CLong}                          |7250:----|


-------------------------------------------------------------------------------
-- *** CURLftpssl
-------------------------------------------------------------------------------
#{cconst CURLFTPSSL_CCC_NONE   , CLong}
#{cconst CURLFTPSSL_CCC_PASSIVE, CLong}
#{cconst CURLFTPSSL_CCC_ACTIVE , CLong}
 

-------------------------------------------------------------------------------
-- *** CURLftpauth
-------------------------------------------------------------------------------
#{cconst CURLFTPAUTH_DEFAULT, CLong}
#{cconst CURLFTPAUTH_SSL    , CLong}
#{cconst CURLFTPAUTH_TLS    , CLong}


-------------------------------------------------------------------------------
-- *** CURLftpcreate
-------------------------------------------------------------------------------
#{cconst CURLFTP_CREATE_DIR_NONE , CLong}
#{cconst CURLFTP_CREATE_DIR      , CLong}
#{cconst CURLFTP_CREATE_DIR_RETRY, CLong}


-------------------------------------------------------------------------------
-- *** CURLftpmethod
-------------------------------------------------------------------------------
#{cconst CURLFTPMETHOD_DEFAULT  , CLong}
#{cconst CURLFTPMETHOD_MULTICWD , CLong}
#{cconst CURLFTPMETHOD_NOCWD    , CLong}
#{cconst CURLFTPMETHOD_SINGLECWD, CLong}


-------------------------------------------------------------------------------
-- *** CURLproto
-------------------------------------------------------------------------------
#{cconst CURLPROTO_HTTP  , CLong}
#{cconst CURLPROTO_HTTPS , CLong}
#{cconst CURLPROTO_FTP   , CLong}
#{cconst CURLPROTO_FTPS  , CLong}
#{cconst CURLPROTO_SCP   , CLong}
#{cconst CURLPROTO_SFTP  , CLong}
#{cconst CURLPROTO_TELNET, CLong}
#{cconst CURLPROTO_LDAP  , CLong}
#{cconst CURLPROTO_LDAPS , CLong}
#{cconst CURLPROTO_DICT  , CLong}
#{cconst CURLPROTO_FILE  , CLong}
#{cconst CURLPROTO_TFTP  , CLong}
#{cconst CURLPROTO_IMAP  , CLong}
#{cconst CURLPROTO_IMAPS , CLong}
#{cconst CURLPROTO_POP3  , CLong}
#{cconst CURLPROTO_POP3S , CLong}
#{cconst CURLPROTO_SMTP  , CLong}
#{cconst CURLPROTO_SMTPS , CLong}
#{cconst CURLPROTO_RTSP  , CLong}
#{cconst CURLPROTO_RTMP  , CLong} |7210:----|
#{cconst CURLPROTO_RTMPT , CLong} |7210:----|
#{cconst CURLPROTO_RTMPE , CLong} |7210:----|
#{cconst CURLPROTO_RTMPTE, CLong} |7210:----|
#{cconst CURLPROTO_RTMPS , CLong} |7210:----|
#{cconst CURLPROTO_RTMPTS, CLong} |7210:----|
#{cconst CURLPROTO_GOPHER, CLong} |7212:----|
#{cconst CURLPROTO_ALL   , CLong}


-------------------------------------------------------------------------------
-- *** CURLoption
-------------------------------------------------------------------------------
newtype CCURLoption'CLong   = CCURLoption'CLong   CInt deriving (Eq, Show)
newtype CCURLoption'Int64   = CCURLoption'Int64   CInt deriving (Eq, Show)
newtype CCURLoption'CString = CCURLoption'CString CInt deriving (Eq, Show)
newtype CCURLoption'CFile   = CCURLoption'CFile   CInt deriving (Eq, Show)
newtype CCURLoption'SList   = CCURLoption'SList   CInt deriving (Eq, Show)
newtype CCURLoption'HTTPP   = CCURLoption'HTTPP   CInt deriving (Eq, Show)
newtype CCURLoption'CURLSH  = CCURLoption'CURLSH  CInt deriving (Eq, Show)
newtype CCURLoption'UsrPtr  = CCURLoption'UsrPtr  CInt deriving (Eq, Show)
newtype CCURLoption'FunPtr  = CCURLoption'FunPtr  CInt deriving (Eq, Show)

#define hsc_curlopt(name, type) \
  printf("c" #name " :: CCURLoption'" #type "\n"); \
  printf("c" #name " =  CCURLoption'" #type " "); hsc_const(name);

#{curlopt CURLOPT_FILE                       , CFile   }
#{curlopt CURLOPT_URL                        , CString }
#{curlopt CURLOPT_PORT                       , CLong   }
#{curlopt CURLOPT_PROXY                      , CString }
#{curlopt CURLOPT_USERPWD                    , CString }
#{curlopt CURLOPT_PROXYUSERPWD               , CString }
#{curlopt CURLOPT_RANGE                      , CString }
#{curlopt CURLOPT_INFILE                     , CFile   }
#{curlopt CURLOPT_ERRORBUFFER                , CString }
#{curlopt CURLOPT_WRITEFUNCTION              , FunPtr  }
#{curlopt CURLOPT_READFUNCTION               , FunPtr  }
#{curlopt CURLOPT_TIMEOUT                    , CLong   }
#{curlopt CURLOPT_INFILESIZE                 , CLong   }
#{curlopt CURLOPT_POSTFIELDS                 , CString }  -- UsrPtr??
#{curlopt CURLOPT_REFERER                    , CString }
#{curlopt CURLOPT_FTPPORT                    , CString }
#{curlopt CURLOPT_USERAGENT                  , CString }
#{curlopt CURLOPT_LOW_SPEED_LIMIT            , CLong   }
#{curlopt CURLOPT_LOW_SPEED_TIME             , CLong   }
#{curlopt CURLOPT_RESUME_FROM                , CLong   }
#{curlopt CURLOPT_COOKIE                     , CString }
#{curlopt CURLOPT_HTTPHEADER                 , SList   }
#{curlopt CURLOPT_HTTPPOST                   , HTTPP   }
#{curlopt CURLOPT_SSLCERT                    , CString }
#{curlopt CURLOPT_KEYPASSWD                  , CString }
#{curlopt CURLOPT_CRLF                       , CLong   }
#{curlopt CURLOPT_QUOTE                      , SList   }
#{curlopt CURLOPT_WRITEHEADER                , CFile   }  -- UsrPtr??
#{curlopt CURLOPT_COOKIEFILE                 , CString }
#{curlopt CURLOPT_SSLVERSION                 , CLong   }
#{curlopt CURLOPT_TIMECONDITION              , CLong   }
#{curlopt CURLOPT_TIMEVALUE                  , CLong   }
#{curlopt CURLOPT_CUSTOMREQUEST              , CString }
#{curlopt CURLOPT_STDERR                     , CFile   }
#{curlopt CURLOPT_POSTQUOTE                  , SList   }
{-# DEPRECATED cCURLOPT_WRITEINFO                 "" #-} |7220:----|
#{curlopt CURLOPT_WRITEINFO                  , CString }
#{curlopt CURLOPT_VERBOSE                    , CLong   }
#{curlopt CURLOPT_HEADER                     , CLong   }
#{curlopt CURLOPT_NOPROGRESS                 , CLong   }
#{curlopt CURLOPT_NOBODY                     , CLong   }
#{curlopt CURLOPT_FAILONERROR                , CLong   }
#{curlopt CURLOPT_UPLOAD                     , CLong   }
#{curlopt CURLOPT_POST                       , CLong   }
#{curlopt CURLOPT_DIRLISTONLY                , CLong   }
#{curlopt CURLOPT_APPEND                     , CLong   }
#{curlopt CURLOPT_NETRC                      , CLong   }
#{curlopt CURLOPT_FOLLOWLOCATION             , CLong   }
#{curlopt CURLOPT_TRANSFERTEXT               , CLong   }
#{curlopt CURLOPT_PUT                        , CLong   }
#{curlopt CURLOPT_PROGRESSFUNCTION           , FunPtr  }
#{curlopt CURLOPT_PROGRESSDATA               , UsrPtr  }
#{curlopt CURLOPT_AUTOREFERER                , CLong   }
#{curlopt CURLOPT_PROXYPORT                  , CLong   }
#{curlopt CURLOPT_POSTFIELDSIZE              , CLong   }
#{curlopt CURLOPT_HTTPPROXYTUNNEL            , CLong   }
#{curlopt CURLOPT_INTERFACE                  , CString }
#{curlopt CURLOPT_KRBLEVEL                   , CString }
#{curlopt CURLOPT_SSL_VERIFYPEER             , CLong   }
#{curlopt CURLOPT_CAINFO                     , CString }
#{curlopt CURLOPT_MAXREDIRS                  , CLong   }
#{curlopt CURLOPT_FILETIME                   , CLong   }
#{curlopt CURLOPT_TELNETOPTIONS              , SList   }
#{curlopt CURLOPT_MAXCONNECTS                , CLong   }
{-# DEPRECATED cCURLOPT_CLOSEPOLICY               "" #-} |7217:----|
#{curlopt CURLOPT_CLOSEPOLICY                , CLong   }
#{curlopt CURLOPT_FRESH_CONNECT              , CLong   }
#{curlopt CURLOPT_FORBID_REUSE               , CLong   }
#{curlopt CURLOPT_RANDOM_FILE                , CString }
#{curlopt CURLOPT_EGDSOCKET                  , CString }
#{curlopt CURLOPT_CONNECTTIMEOUT             , CLong   }
#{curlopt CURLOPT_HEADERFUNCTION             , FunPtr  }
#{curlopt CURLOPT_HTTPGET                    , CLong   }
#{curlopt CURLOPT_SSL_VERIFYHOST             , CLong   }
#{curlopt CURLOPT_COOKIEJAR                  , CString }
#{curlopt CURLOPT_SSL_CIPHER_LIST            , CString }
#{curlopt CURLOPT_HTTP_VERSION               , CLong   }
#{curlopt CURLOPT_FTP_USE_EPSV               , CLong   }
#{curlopt CURLOPT_SSLCERTTYPE                , CString }
#{curlopt CURLOPT_SSLKEY                     , CString }
#{curlopt CURLOPT_SSLKEYTYPE                 , CString }
#{curlopt CURLOPT_SSLENGINE                  , CString }
#{curlopt CURLOPT_SSLENGINE_DEFAULT          , CLong   }
{-# DEPRECATED cCURLOPT_DNS_USE_GLOBAL_CACHE      "" #-} |7220:----|
#{curlopt CURLOPT_DNS_USE_GLOBAL_CACHE       , CLong   }
#{curlopt CURLOPT_DNS_CACHE_TIMEOUT          , CLong   }
#{curlopt CURLOPT_PREQUOTE                   , SList   }
#{curlopt CURLOPT_DEBUGFUNCTION              , FunPtr  }
#{curlopt CURLOPT_DEBUGDATA                  , UsrPtr  }
#{curlopt CURLOPT_COOKIESESSION              , CLong   }
#{curlopt CURLOPT_CAPATH                     , CString }
#{curlopt CURLOPT_BUFFERSIZE                 , CLong   }
#{curlopt CURLOPT_NOSIGNAL                   , CLong   }
#{curlopt CURLOPT_SHARE                      , CURLSH  }
#{curlopt CURLOPT_PROXYTYPE                  , CLong   }
#{curlopt CURLOPT_ENCODING                   , CString } |----:7215|
#{curlopt CURLOPT_ACCEPT_ENCODING            , CString } |7216:----|
#{curlopt CURLOPT_PRIVATE                    , UsrPtr  }
#{curlopt CURLOPT_HTTP200ALIASES             , SList   }
#{curlopt CURLOPT_UNRESTRICTED_AUTH          , CLong   }
#{curlopt CURLOPT_FTP_USE_EPRT               , CLong   }
#{curlopt CURLOPT_HTTPAUTH                   , CLong   }
#{curlopt CURLOPT_SSL_CTX_FUNCTION           , FunPtr  }
#{curlopt CURLOPT_SSL_CTX_DATA               , UsrPtr  }
#{curlopt CURLOPT_FTP_CREATE_MISSING_DIRS    , CLong   }
#{curlopt CURLOPT_PROXYAUTH                  , CLong   }
#{curlopt CURLOPT_FTP_RESPONSE_TIMEOUT       , CLong   }
#{curlopt CURLOPT_IPRESOLVE                  , CLong   }
#{curlopt CURLOPT_MAXFILESIZE                , CLong   }
#{curlopt CURLOPT_INFILESIZE_LARGE           , Int64   }
#{curlopt CURLOPT_RESUME_FROM_LARGE          , Int64   }
#{curlopt CURLOPT_MAXFILESIZE_LARGE          , Int64   }
#{curlopt CURLOPT_NETRC_FILE                 , CString }
#{curlopt CURLOPT_USE_SSL                    , CLong   }
#{curlopt CURLOPT_POSTFIELDSIZE_LARGE        , Int64   }
#{curlopt CURLOPT_TCP_NODELAY                , CLong   }
#{curlopt CURLOPT_FTPSSLAUTH                 , CLong   }
#{curlopt CURLOPT_IOCTLFUNCTION              , FunPtr  }
#{curlopt CURLOPT_IOCTLDATA                  , UsrPtr  }
#{curlopt CURLOPT_FTP_ACCOUNT                , CString }
#{curlopt CURLOPT_COOKIELIST                 , CString }
#{curlopt CURLOPT_IGNORE_CONTENT_LENGTH      , CLong   }
#{curlopt CURLOPT_FTP_SKIP_PASV_IP           , CLong   }
#{curlopt CURLOPT_FTP_FILEMETHOD             , CLong   }
#{curlopt CURLOPT_LOCALPORT                  , CLong   }
#{curlopt CURLOPT_LOCALPORTRANGE             , CLong   }
#{curlopt CURLOPT_CONNECT_ONLY               , CLong   }
#{curlopt CURLOPT_CONV_FROM_NETWORK_FUNCTION , FunPtr  }
#{curlopt CURLOPT_CONV_TO_NETWORK_FUNCTION   , FunPtr  }
#{curlopt CURLOPT_CONV_FROM_UTF8_FUNCTION    , FunPtr  }
#{curlopt CURLOPT_MAX_SEND_SPEED_LARGE       , Int64   }
#{curlopt CURLOPT_MAX_RECV_SPEED_LARGE       , Int64   }
#{curlopt CURLOPT_FTP_ALTERNATIVE_TO_USER    , CString }
#{curlopt CURLOPT_SOCKOPTFUNCTION            , FunPtr  }
#{curlopt CURLOPT_SOCKOPTDATA                , UsrPtr  }
#{curlopt CURLOPT_SSL_SESSIONID_CACHE        , CLong   }
#{curlopt CURLOPT_SSH_AUTH_TYPES             , CLong   }
#{curlopt CURLOPT_SSH_PUBLIC_KEYFILE         , CString }
#{curlopt CURLOPT_SSH_PRIVATE_KEYFILE        , CString }
#{curlopt CURLOPT_FTP_SSL_CCC                , CLong   }
#{curlopt CURLOPT_TIMEOUT_MS                 , CLong   }
#{curlopt CURLOPT_CONNECTTIMEOUT_MS          , CLong   }
#{curlopt CURLOPT_HTTP_TRANSFER_DECODING     , CLong   }
#{curlopt CURLOPT_HTTP_CONTENT_DECODING      , CLong   }
#{curlopt CURLOPT_NEW_FILE_PERMS             , CLong   }
#{curlopt CURLOPT_NEW_DIRECTORY_PERMS        , CLong   }
#{curlopt CURLOPT_POSTREDIR                  , CLong   }
#{curlopt CURLOPT_SSH_HOST_PUBLIC_KEY_MD5    , CString }
#{curlopt CURLOPT_OPENSOCKETFUNCTION         , FunPtr  }
#{curlopt CURLOPT_OPENSOCKETDATA             , UsrPtr  }
#{curlopt CURLOPT_COPYPOSTFIELDS             , CString }
#{curlopt CURLOPT_PROXY_TRANSFER_MODE        , CLong   }
#{curlopt CURLOPT_SEEKFUNCTION               , FunPtr  }
#{curlopt CURLOPT_SEEKDATA                   , UsrPtr  }
#{curlopt CURLOPT_CRLFILE                    , CString }
#{curlopt CURLOPT_ISSUERCERT                 , CString }
#{curlopt CURLOPT_ADDRESS_SCOPE              , CLong   }
#{curlopt CURLOPT_CERTINFO                   , CLong   }
#{curlopt CURLOPT_USERNAME                   , CString }
#{curlopt CURLOPT_PASSWORD                   , CString }
#{curlopt CURLOPT_PROXYUSERNAME              , CString }
#{curlopt CURLOPT_PROXYPASSWORD              , CString }
#{curlopt CURLOPT_NOPROXY                    , CString }
#{curlopt CURLOPT_TFTP_BLKSIZE               , CLong   }
#{curlopt CURLOPT_SOCKS5_GSSAPI_SERVICE      , CString }
#{curlopt CURLOPT_SOCKS5_GSSAPI_NEC          , CLong   }
#{curlopt CURLOPT_PROTOCOLS                  , CLong   }
#{curlopt CURLOPT_REDIR_PROTOCOLS            , CLong   }
#{curlopt CURLOPT_SSH_KNOWNHOSTS             , CString }
#{curlopt CURLOPT_SSH_KEYFUNCTION            , FunPtr  }
#{curlopt CURLOPT_SSH_KEYDATA                , UsrPtr  }
#{curlopt CURLOPT_MAIL_FROM                  , CString }
#{curlopt CURLOPT_MAIL_RCPT                  , SList   }
#{curlopt CURLOPT_FTP_USE_PRET               , CLong   }
#{curlopt CURLOPT_RTSP_REQUEST               , CLong   }
#{curlopt CURLOPT_RTSP_SESSION_ID            , CString }
#{curlopt CURLOPT_RTSP_STREAM_URI            , CString }
#{curlopt CURLOPT_RTSP_TRANSPORT             , CString }
#{curlopt CURLOPT_RTSP_CLIENT_CSEQ           , CLong   }
#{curlopt CURLOPT_RTSP_SERVER_CSEQ           , CLong   }
#{curlopt CURLOPT_INTERLEAVEDATA             , UsrPtr  }
#{curlopt CURLOPT_INTERLEAVEFUNCTION         , FunPtr  }
#{curlopt CURLOPT_WILDCARDMATCH              , CLong   } |7210:----|
#{curlopt CURLOPT_CHUNK_BGN_FUNCTION         , FunPtr  } |7210:----|
#{curlopt CURLOPT_CHUNK_END_FUNCTION         , FunPtr  } |7210:----|
#{curlopt CURLOPT_FNMATCH_FUNCTION           , FunPtr  } |7210:----|
#{curlopt CURLOPT_CHUNK_DATA                 , UsrPtr  } |7210:----|
#{curlopt CURLOPT_FNMATCH_DATA               , UsrPtr  } |7210:----|
#{curlopt CURLOPT_RESOLVE                    , SList   } |7213:----|
#{curlopt CURLOPT_TLSAUTH_USERNAME           , CString } |7214:----|
#{curlopt CURLOPT_TLSAUTH_PASSWORD           , CString } |7214:----|
#{curlopt CURLOPT_TLSAUTH_TYPE               , CString } |7214:----|
#{curlopt CURLOPT_TRANSFER_ENCODING          , CLong   } |7216:----|
#{curlopt CURLOPT_CLOSESOCKETFUNCTION        , FunPtr  } |7217:----|
#{curlopt CURLOPT_CLOSESOCKETDATA            , UsrPtr  } |7217:----|
#{curlopt CURLOPT_GSSAPI_DELEGATION          , CLong   } |7220:----|
#{curlopt CURLOPT_DNS_SERVERS                , CString } |7240:----|
#{curlopt CURLOPT_ACCEPTTIMEOUT_MS           , CLong   } |7240:----|
#{curlopt CURLOPT_TCP_KEEPALIVE              , CLong   } |7250:----|
#{curlopt CURLOPT_TCP_KEEPIDLE               , CLong   } |7250:----|
#{curlopt CURLOPT_TCP_KEEPINTVL              , CLong   } |7250:----|
#{curlopt CURLOPT_SSL_OPTIONS                , CLong   } |7250:----|
#{curlopt CURLOPT_MAIL_AUTH                  , CString } |7250:----|

#{curlopt CURLOPT_WRITEDATA                  , UsrPtr  }
#{curlopt CURLOPT_READDATA                   , UsrPtr  }
#{curlopt CURLOPT_HEADERDATA                 , UsrPtr  }
#{curlopt CURLOPT_RTSPHEADER                 , SList   }


-------------------------------------------------------------------------------
-- *** CURLipresolve
-------------------------------------------------------------------------------
#{cconst CURL_IPRESOLVE_WHATEVER, CLong}
#{cconst CURL_IPRESOLVE_V4      , CLong}
#{cconst CURL_IPRESOLVE_V6      , CLong}


-------------------------------------------------------------------------------
-- *** CURLhttpver
-------------------------------------------------------------------------------
#{cconst CURL_HTTP_VERSION_NONE, CLong}
#{cconst CURL_HTTP_VERSION_1_0 , CLong}
#{cconst CURL_HTTP_VERSION_1_1 , CLong}


-------------------------------------------------------------------------------
-- *** CURLrtspreq
-------------------------------------------------------------------------------
#{cconst CURL_RTSPREQ_NONE         , CLong}
#{cconst CURL_RTSPREQ_OPTIONS      , CLong}
#{cconst CURL_RTSPREQ_DESCRIBE     , CLong}
#{cconst CURL_RTSPREQ_ANNOUNCE     , CLong}
#{cconst CURL_RTSPREQ_SETUP        , CLong}
#{cconst CURL_RTSPREQ_PLAY         , CLong}
#{cconst CURL_RTSPREQ_PAUSE        , CLong}
#{cconst CURL_RTSPREQ_TEARDOWN     , CLong}
#{cconst CURL_RTSPREQ_GET_PARAMETER, CLong}
#{cconst CURL_RTSPREQ_SET_PARAMETER, CLong}
#{cconst CURL_RTSPREQ_RECORD       , CLong}
#{cconst CURL_RTSPREQ_RECEIVE      , CLong}


-------------------------------------------------------------------------------
-- *** CURLnetrc
-------------------------------------------------------------------------------
#{cconst CURL_NETRC_IGNORED , CLong}
#{cconst CURL_NETRC_OPTIONAL, CLong}
#{cconst CURL_NETRC_REQUIRED, CLong}


-------------------------------------------------------------------------------
-- *** CURLsslver
-------------------------------------------------------------------------------
#{cconst CURL_SSLVERSION_DEFAULT, CLong}
#{cconst CURL_SSLVERSION_TLSv1  , CLong}
#{cconst CURL_SSLVERSION_SSLv2  , CLong}
#{cconst CURL_SSLVERSION_SSLv3  , CLong}


-------------------------------------------------------------------------------
-- *** CURLtlsauth
-------------------------------------------------------------------------------
#{cconst CURL_TLSAUTH_NONE, CLong}                               |7214:----|
#{cconst CURL_TLSAUTH_SRP , CLong}                               |7214:----|


-------------------------------------------------------------------------------
-- *** CURLredir
-------------------------------------------------------------------------------
#{cconst CURL_REDIR_GET_ALL , CLong}
#{cconst CURL_REDIR_POST_301, CLong}
#{cconst CURL_REDIR_POST_302, CLong}
#{cconst CURL_REDIR_POST_ALL, CLong}


-------------------------------------------------------------------------------
-- *** CURLtimecond
-------------------------------------------------------------------------------
#{cconst CURL_TIMECOND_NONE        , CLong}
#{cconst CURL_TIMECOND_IFMODSINCE  , CLong}
#{cconst CURL_TIMECOND_IFUNMODSINCE, CLong}
#{cconst CURL_TIMECOND_LASTMOD     , CLong}


-------------------------------------------------------------------------------
-- ** Functions
-------------------------------------------------------------------------------
{-# DEPRECATED ccurl_strequal "" #-}
foreign import ccall "curl_strequal"
  ccurl_strequal
    :: Ptr CChar
    -> Ptr CChar
    -> IO CInt


-------------------------------------------------------------------------------
{-# DEPRECATED ccurl_strnequal "" #-}
foreign import ccall "curl_strnequal"
  ccurl_strnequal
    :: Ptr CChar
    -> Ptr CChar
    -> CSize
    -> IO CInt


-------------------------------------------------------------------------------
-- ** CURL_forms
-------------------------------------------------------------------------------
newtype CCURLformoption = CCURLformoption CInt deriving (Eq, Show)
 
-- #{symbol CURLFORM_COPYNAME      , CURLformoption}
-- #{symbol CURLFORM_PTRNAME       , CURLformoption}
-- #{symbol CURLFORM_NAMELENGTH    , CURLformoption}
-- #{symbol CURLFORM_COPYCONTENTS  , CURLformoption}
-- #{symbol CURLFORM_PTRCONTENTS   , CURLformoption}
-- #{symbol CURLFORM_CONTENTSLENGTH, CURLformoption}
-- #{symbol CURLFORM_FILECONTENT   , CURLformoption}
-- #{symbol CURLFORM_ARRAY         , CURLformoption}
-- #{symbol CURLFORM_FILE          , CURLformoption}
-- #{symbol CURLFORM_BUFFER        , CURLformoption}
-- #{symbol CURLFORM_BUFFERPTR     , CURLformoption}
-- #{symbol CURLFORM_BUFFERLENGTH  , CURLformoption}
-- #{symbol CURLFORM_CONTENTTYPE   , CURLformoption}
-- #{symbol CURLFORM_CONTENTHEADER , CURLformoption}
-- #{symbol CURLFORM_FILENAME      , CURLformoption}
-- #{symbol CURLFORM_END           , CURLformoption}
-- #{symbol CURLFORM_STREAM        , CURLformoption}


-------------------------------------------------------------------------------
data CCURL_forms = CCURL_forms
  { ccurl_forms_option :: CCURLformoption
  , ccurl_forms_value  :: Ptr CChar
  } deriving (Show)

-- instance Storable CCURL_forms where
--   sizeOf _    = #{size    struct curl_forms}
--   alignment _ = #{alignof struct curl_forms}
--   poke _ _    = undefined
--   peek _      = undefined


-------------------------------------------------------------------------------
newtype CCURLformcode = CCURLformcode CInt deriving (Eq, Show)

#{symbol CURL_FORMADD_OK            , CURLformcode}
#{symbol CURL_FORMADD_MEMORY        , CURLformcode}
#{symbol CURL_FORMADD_OPTION_TWICE  , CURLformcode}
#{symbol CURL_FORMADD_NULL          , CURLformcode}
#{symbol CURL_FORMADD_UNKNOWN_OPTION, CURLformcode}
#{symbol CURL_FORMADD_INCOMPLETE    , CURLformcode}
#{symbol CURL_FORMADD_ILLEGAL_ARRAY , CURLformcode}
#{symbol CURL_FORMADD_DISABLED      , CURLformcode}


-- ----------------------------------------------------------------------------
-- CURL_EXTERN CURLFORMcode curl_formadd(struct curl_httppost **httppost,
--                                       struct curl_httppost **last_post,
--                                       ...);


-------------------------------------------------------------------------------
type CCURL_formget_callback
  = Ptr () -> Ptr CChar -> CSize -> IO CSize

foreign import ccall "wrapper"
  wrap_ccurl_formget_callback
    :: CCURL_formget_callback
    -> IO (FunPtr CCURL_formget_callback)


-------------------------------------------------------------------------------
foreign import ccall "curl_formget"
  ccurl_formget
    :: Ptr CCURL_httppost
    -> Ptr ()
    -> FunPtr CCURL_formget_callback
    -> IO CInt


-------------------------------------------------------------------------------
foreign import ccall "curl_formfree"
  ccurl_formfree
    :: Ptr CCURL_httppost
    -> IO ()


-------------------------------------------------------------------------------
-- ** Functions
-------------------------------------------------------------------------------
{-# DEPRECATED ccurl_getenv "" #-}
foreign import ccall "curl_getenv"
  ccurl_getenv
    :: Ptr CChar
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_version"
  ccurl_version
    :: IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_escape"
  ccurl_easy_escape
    :: Ptr CCURL
    -> Ptr CChar
    -> CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_escape"
  ccurl_escape
    :: Ptr CChar
    -> CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_unescape"
  ccurl_easy_unescape
    :: Ptr CCURL
    -> Ptr CChar
    -> CInt
    -> Ptr CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_unescape"
  ccurl_unescape
    :: Ptr CChar
    -> CInt
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_free"
  ccurl_free
    :: Ptr a
    -> IO ()


-------------------------------------------------------------------------------
foreign import ccall "curl_global_init"
  ccurl_global_init
    :: CLong
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_global_init_mem"
  ccurl_global_init_mem
    :: CLong
    -> FunPtr CCURL_malloc_callback
    -> FunPtr CCURL_free_callback
    -> FunPtr CCURL_realloc_callback
    -> FunPtr CCURL_strdup_callback
    -> FunPtr CCURL_calloc_callback
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_global_cleanup"
  ccurl_global_cleanup
    :: IO ()


-------------------------------------------------------------------------------
-- ** CURL_slist
-------------------------------------------------------------------------------
data CCURL_slist = CCURL_slist
  { ccurl_slist_data :: Ptr CChar
  , ccurl_slist_next :: Ptr CCURL_slist
  } deriving (Show)

instance Storable CCURL_slist where
  sizeOf _    = #{size    struct curl_slist}
  alignment _ = #{alignof struct curl_slist}
  poke _ _    = undefined
  peek ptr    = CCURL_slist
    <$> #{peek struct curl_slist, data} ptr
    <*> #{peek struct curl_slist, next} ptr

 
-------------------------------------------------------------------------------
-- ** Functions
-------------------------------------------------------------------------------
foreign import ccall "curl_slist_append"
  ccurl_slist_append
    :: Ptr CCURL_slist
    -> Ptr CChar
    -> IO (Ptr CCURL_slist)


-------------------------------------------------------------------------------
foreign import ccall "curl_slist_free_all"
  ccurl_slist_free_all
    :: Ptr CCURL_slist
    -> IO ()

 
-------------------------------------------------------------------------------
foreign import ccall "curl_getdate"
  ccurl_getdate
    :: Ptr CChar
    -> Ptr CTime
    -> IO CTime


-------------------------------------------------------------------------------
-- ** CURL_certinfo
-------------------------------------------------------------------------------
data CCURL_certinfo = CCURL_certinfo
  { ccurl_certinfo_num_of_certs :: CInt
  , ccurl_certinfo_certinfo     :: Ptr (Ptr CCURL_slist)
  } deriving (Show)

instance Storable CCURL_certinfo where
  sizeOf _    = #{size    struct curl_certinfo}
  alignment _ = #{alignof struct curl_certinfo}
  poke _ _    = undefined
  peek ptr    = CCURL_certinfo
    <$> #{peek struct curl_certinfo, num_of_certs} ptr
    <*> #{peek struct curl_certinfo, certinfo    } ptr


-------------------------------------------------------------------------------
-- ** Constants
-------------------------------------------------------------------------------
-- *** CURLinfo
-------------------------------------------------------------------------------
newtype CCURLinfo'CString = CCURLinfo'CString CInt deriving (Eq, Show)
newtype CCURLinfo'CDouble = CCURLinfo'CDouble CInt deriving (Eq, Show)
newtype CCURLinfo'CLong   = CCURLinfo'CLong   CInt deriving (Eq, Show)
newtype CCURLinfo'SList   = CCURLinfo'SList   CInt deriving (Eq, Show)
newtype CCURLinfo'CertI   = CCURLinfo'CertI   CInt deriving (Eq, Show)

#define hsc_curlinfo(name, type) \
  printf("c" #name " :: CCURLinfo'" #type "\n"); \
  printf("c" #name " =  CCURLinfo'" #type " "); hsc_const(name);

#{curlinfo CURLINFO_EFFECTIVE_URL          , CString}
#{curlinfo CURLINFO_RESPONSE_CODE          , CLong  }
#{curlinfo CURLINFO_TOTAL_TIME             , CDouble}
#{curlinfo CURLINFO_NAMELOOKUP_TIME        , CDouble}
#{curlinfo CURLINFO_CONNECT_TIME           , CDouble}
#{curlinfo CURLINFO_PRETRANSFER_TIME       , CDouble}
#{curlinfo CURLINFO_SIZE_UPLOAD            , CDouble}
#{curlinfo CURLINFO_SIZE_DOWNLOAD          , CDouble}
#{curlinfo CURLINFO_SPEED_DOWNLOAD         , CDouble}
#{curlinfo CURLINFO_SPEED_UPLOAD           , CDouble}
#{curlinfo CURLINFO_HEADER_SIZE            , CLong  }
#{curlinfo CURLINFO_REQUEST_SIZE           , CLong  }
#{curlinfo CURLINFO_SSL_VERIFYRESULT       , CLong  }
#{curlinfo CURLINFO_FILETIME               , CLong  }
#{curlinfo CURLINFO_CONTENT_LENGTH_DOWNLOAD, CDouble}
#{curlinfo CURLINFO_CONTENT_LENGTH_UPLOAD  , CDouble}
#{curlinfo CURLINFO_STARTTRANSFER_TIME     , CDouble}
#{curlinfo CURLINFO_CONTENT_TYPE           , CString}
#{curlinfo CURLINFO_REDIRECT_TIME          , CDouble}
#{curlinfo CURLINFO_REDIRECT_COUNT         , CLong  }
#{curlinfo CURLINFO_PRIVATE                , CString}
#{curlinfo CURLINFO_HTTP_CONNECTCODE       , CLong  }
#{curlinfo CURLINFO_HTTPAUTH_AVAIL         , CLong  }
#{curlinfo CURLINFO_PROXYAUTH_AVAIL        , CLong  }
#{curlinfo CURLINFO_OS_ERRNO               , CLong  }
#{curlinfo CURLINFO_NUM_CONNECTS           , CLong  }
#{curlinfo CURLINFO_SSL_ENGINES            , SList  }
#{curlinfo CURLINFO_COOKIELIST             , SList  }
#{curlinfo CURLINFO_LASTSOCKET             , CLong  }
#{curlinfo CURLINFO_FTP_ENTRY_PATH         , CString}
#{curlinfo CURLINFO_REDIRECT_URL           , CString}
#{curlinfo CURLINFO_PRIMARY_IP             , CString}
#{curlinfo CURLINFO_APPCONNECT_TIME        , CDouble}
#{curlinfo CURLINFO_CERTINFO               , CertI  }
#{curlinfo CURLINFO_CONDITION_UNMET        , CLong  }
#{curlinfo CURLINFO_RTSP_SESSION_ID        , CString}
#{curlinfo CURLINFO_RTSP_CLIENT_CSEQ       , CLong  }
#{curlinfo CURLINFO_RTSP_SERVER_CSEQ       , CLong  }
#{curlinfo CURLINFO_RTSP_CSEQ_RECV         , CLong  }
#{curlinfo CURLINFO_PRIMARY_PORT           , CLong  } |7210:----|
#{curlinfo CURLINFO_LOCAL_IP               , CString} |7210:----|
#{curlinfo CURLINFO_LOCAL_PORT             , CLong  } |7210:----|


-------------------------------------------------------------------------------
-- *** CURLclosepol
-------------------------------------------------------------------------------
#{cconst CURLCLOSEPOLICY_NONE               , CLong}
#{cconst CURLCLOSEPOLICY_OLDEST             , CLong}
#{cconst CURLCLOSEPOLICY_LEAST_RECENTLY_USED, CLong}
#{cconst CURLCLOSEPOLICY_LEAST_TRAFFIC      , CLong}
#{cconst CURLCLOSEPOLICY_SLOWEST            , CLong}
#{cconst CURLCLOSEPOLICY_CALLBACK           , CLong}


-------------------------------------------------------------------------------
-- *** CURLglobal
-------------------------------------------------------------------------------
#{cconst CURL_GLOBAL_SSL    , CLong}
#{cconst CURL_GLOBAL_WIN32  , CLong}
#{cconst CURL_GLOBAL_ALL    , CLong}
#{cconst CURL_GLOBAL_NOTHING, CLong}
#{cconst CURL_GLOBAL_DEFAULT, CLong}


-------------------------------------------------------------------------------
-- ** Share interface
-------------------------------------------------------------------------------
newtype CCURL_lock_data = CCURL_lock_data CInt deriving (Eq, Show)

#{symbol CURL_LOCK_DATA_COOKIE     , CURL_lock_data}
#{symbol CURL_LOCK_DATA_DNS        , CURL_lock_data}
#{symbol CURL_LOCK_DATA_SSL_SESSION, CURL_lock_data}
#{symbol CURL_LOCK_DATA_CONNECT    , CURL_lock_data}
 

-------------------------------------------------------------------------------
newtype CCURL_lock_access = CCURL_lock_access CInt deriving (Eq, Show)

#{symbol CURL_LOCK_ACCESS_NONE  , CURL_lock_access}
#{symbol CURL_LOCK_ACCESS_SHARED, CURL_lock_access}
#{symbol CURL_LOCK_ACCESS_SINGLE, CURL_lock_access}


-------------------------------------------------------------------------------
type CCURL_lock_function
  = Ptr CCURL -> CCURL_lock_data -> CCURL_lock_access -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrap_ccurl_lock_function
    :: CCURL_lock_function
    -> IO (FunPtr CCURL_lock_function)


-------------------------------------------------------------------------------
type CCURL_unlock_function
  = Ptr CCURL -> CCURL_lock_data -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrap_ccurl_unlock_function
    :: CCURL_unlock_function
    -> IO (FunPtr CCURL_unlock_function)


-------------------------------------------------------------------------------
data CCURLSH

 
-------------------------------------------------------------------------------
newtype CCURLSHcode = CCURLSHcode CInt deriving (Eq, Show)

#{symbol CURLSHE_OK          , CURLSHcode}
#{symbol CURLSHE_BAD_OPTION  , CURLSHcode}
#{symbol CURLSHE_IN_USE      , CURLSHcode}
#{symbol CURLSHE_INVALID     , CURLSHcode}
#{symbol CURLSHE_NOMEM       , CURLSHcode}
#{symbol CURLSHE_NOT_BUILT_IN, CURLSHcode} |7230:----|


-------------------------------------------------------------------------------
newtype CCURLSHoption'Lock    = CCURLSHoption'Lock    CInt deriving (Eq, Show)
newtype CCURLSHoption'FLOCK   = CCURLSHoption'FLOCK   CInt deriving (Eq, Show)
newtype CCURLSHoption'FUNLOCK = CCURLSHoption'FUNLOCK CInt deriving (Eq, Show)
newtype CCURLSHoption'UsrPtr  = CCURLSHoption'UsrPtr  CInt deriving (Eq, Show)

#define hsc_curlshopt(name, type) \
  printf("c" #name " :: CCURLSHoption'" #type "\n"); \
  printf("c" #name " =  CCURLSHoption'" #type " "); hsc_const(name);

#{curlshopt CURLSHOPT_SHARE     , Lock   }
#{curlshopt CURLSHOPT_UNSHARE   , Lock   }
#{curlshopt CURLSHOPT_LOCKFUNC  , FLOCK  }
#{curlshopt CURLSHOPT_UNLOCKFUNC, FUNLOCK}
#{curlshopt CURLSHOPT_USERDATA  , UsrPtr }


-------------------------------------------------------------------------------
foreign import ccall "curl_share_init"
  ccurl_share_init
    :: IO (Ptr CCURLSH)


-------------------------------------------------------------------------------
foreign import ccall "curl_share_setopt"
  ccurl_share_setopt'Lock   
    :: Ptr CCURLSH
    -> CCURLSHoption'Lock   
    -> CCURL_lock_data
    -> IO CCURLSHcode

foreign import ccall "curl_share_setopt"
  ccurl_share_setopt'FLOCK
    :: Ptr CCURLSH
    -> CCURLSHoption'FLOCK
    -> FunPtr CCURL_lock_function
    -> IO CCURLSHcode

foreign import ccall "curl_share_setopt"
  ccurl_share_setopt'FUNLOCK
    :: Ptr CCURLSH
    -> CCURLSHoption'FUNLOCK
    -> FunPtr CCURL_unlock_function
    -> IO CCURLSHcode

foreign import ccall "curl_share_setopt"
  ccurl_share_setopt'UsrPtr 
    :: Ptr CCURLSH
    -> CCURLSHoption'UsrPtr 
    -> Ptr ()
    -> IO CCURLSHcode


-------------------------------------------------------------------------------
foreign import ccall "curl_share_cleanup"
  ccurl_share_cleanup
    :: Ptr CCURLSH
    -> IO CCURLSHcode


-------------------------------------------------------------------------------
-- ** CURL_version_info
-------------------------------------------------------------------------------
newtype CCURLversion = CCURLversion CInt deriving (Eq, Show)

instance Storable CCURLversion where
  sizeOf _    = #{size    CURLversion}
  alignment _ = #{alignof CURLversion}
  poke _ _    = undefined
  peek ptr    = CCURLversion <$> peek (castPtr ptr)

#{symbol CURLVERSION_FIRST , CURLversion}
#{symbol CURLVERSION_SECOND, CURLversion}
#{symbol CURLVERSION_THIRD , CURLversion}
#{symbol CURLVERSION_FOURTH, CURLversion}
#{symbol CURLVERSION_NOW   , CURLversion}


-------------------------------------------------------------------------------
data CCURL_version_info_data = CCURL_version_info_data
  { ccurl_version_info_data_age             :: CCURLversion
  , ccurl_version_info_data_version         :: Ptr CChar
  , ccurl_version_info_data_version_num     :: CUInt
  , ccurl_version_info_data_host            :: Ptr CChar
  , ccurl_version_info_data_features        :: CInt
  , ccurl_version_info_data_ssl_version     :: Ptr CChar
  , ccurl_version_info_data_ssl_version_num :: CLong
  , ccurl_version_info_data_libz_version    :: Ptr CChar
  , ccurl_version_info_data_protocols       :: Ptr (Ptr CChar)
  , ccurl_version_info_data_ares            :: Ptr CChar
  , ccurl_version_info_data_ares_num        :: CInt
  , ccurl_version_info_data_libidn          :: Ptr CChar
  , ccurl_version_info_data_iconv_ver_num   :: CInt
  , ccurl_version_info_data_libssh_version  :: Ptr CChar
  } deriving (Show)

instance Storable CCURL_version_info_data where
  sizeOf _    = #{size    curl_version_info_data}
  alignment _ = #{alignof curl_version_info_data}
  poke _ _    = undefined
  peek ptr    = CCURL_version_info_data
    <$> #{peek curl_version_info_data, age            } ptr
    <*> #{peek curl_version_info_data, version        } ptr
    <*> #{peek curl_version_info_data, version_num    } ptr
    <*> #{peek curl_version_info_data, host           } ptr
    <*> #{peek curl_version_info_data, features       } ptr
    <*> #{peek curl_version_info_data, ssl_version    } ptr
    <*> #{peek curl_version_info_data, ssl_version_num} ptr
    <*> #{peek curl_version_info_data, libz_version   } ptr
    <*> #{peek curl_version_info_data, protocols      } ptr
    <*> #{peek curl_version_info_data, ares           } ptr
    <*> #{peek curl_version_info_data, ares_num       } ptr
    <*> #{peek curl_version_info_data, libidn         } ptr
    <*> #{peek curl_version_info_data, iconv_ver_num  } ptr
    <*> #{peek curl_version_info_data, libssh_version } ptr


-------------------------------------------------------------------------------
#{cconst CURL_VERSION_IPV6        , CInt}
#{cconst CURL_VERSION_KERBEROS4   , CInt}
#{cconst CURL_VERSION_SSL         , CInt}
#{cconst CURL_VERSION_LIBZ        , CInt}
#{cconst CURL_VERSION_NTLM        , CInt}
#{cconst CURL_VERSION_GSSNEGOTIATE, CInt}
#{cconst CURL_VERSION_DEBUG       , CInt}
#{cconst CURL_VERSION_ASYNCHDNS   , CInt}
#{cconst CURL_VERSION_SPNEGO      , CInt}
#{cconst CURL_VERSION_LARGEFILE   , CInt}
#{cconst CURL_VERSION_IDN         , CInt}
#{cconst CURL_VERSION_SSPI        , CInt}
#{cconst CURL_VERSION_CONV        , CInt}
#{cconst CURL_VERSION_CURLDEBUG   , CInt}
#{cconst CURL_VERSION_TLSAUTH_SRP , CInt} |7214:----|
#{cconst CURL_VERSION_NTLM_WB     , CInt} |7220:----|

 
-------------------------------------------------------------------------------
-- ** Functions
-------------------------------------------------------------------------------
foreign import ccall "curl_version_info"
  ccurl_version_info
    :: CCURLversion
    -> IO (Ptr CCURL_version_info_data)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_strerror"
  ccurl_easy_strerror
    :: CCURLcode
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_share_strerror"
  ccurl_share_strerror
    :: CCURLSHcode
    -> IO (Ptr CChar)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_pause"
  ccurl_easy_pause
    :: Ptr CCURL
    -> CInt
    -> IO CCURLcode
 
#{cconst CURLPAUSE_RECV     , CInt}
#{cconst CURLPAUSE_RECV_CONT, CInt}
#{cconst CURLPAUSE_SEND     , CInt}
#{cconst CURLPAUSE_SEND_CONT, CInt}
#{cconst CURLPAUSE_ALL      , CInt}
#{cconst CURLPAUSE_CONT     , CInt}


-------------------------------------------------------------------------------
-- * Definitions from \"curl/easy.h\"
-------------------------------------------------------------------------------
foreign import ccall "curl_easy_init"
  ccurl_easy_init
    :: IO (Ptr CCURL)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'CLong
    :: Ptr CCURL
    -> CCURLoption'CLong
    -> CLong
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'Int64
    :: Ptr CCURL
    -> CCURLoption'Int64
    -> CCURL_off_t
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'CString
    :: Ptr CCURL
    -> CCURLoption'CString
    -> Ptr CChar
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'CFile
    :: Ptr CCURL
    -> CCURLoption'CFile
    -> Ptr CFile
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'SList
    :: Ptr CCURL
    -> CCURLoption'SList
    -> Ptr CCURL_slist
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'HTTPP
    :: Ptr CCURL
    -> CCURLoption'HTTPP
    -> Ptr CCURL_httppost
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'CURLSH
    :: Ptr CCURL
    -> CCURLoption'CURLSH
    -> Ptr CCURLSH
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'UsrPtr
    :: Ptr CCURL
    -> CCURLoption'UsrPtr
    -> Ptr ()
    -> IO CCURLcode

foreign import ccall "curl_easy_setopt"
  ccurl_easy_setopt'FunPtr
    :: Ptr CCURL
    -> CCURLoption'FunPtr
    -> FunPtr a
    -> IO CCURLcode

#define hsc_setFunPtrAlias(fn, un, ln) \
  printf("ccurl_easy_setopt'" #fn "\n"); \
  printf("  :: Ptr CCURL -> FunPtr CCURL_" #ln "_callback -> IO CCURLcode\n");\
  printf("ccurl_easy_setopt'" #fn " curl fptr\n"); \
  printf("  = ccurl_easy_setopt'FunPtr curl cCURLOPT_" #un "FUNCTION fptr");

#{setFunPtrAlias FWRITE      , WRITE      , write      }
#{setFunPtrAlias FREAD       , READ       , read       }
#{setFunPtrAlias FPROGRESS   , PROGRESS   , progress   }
#{setFunPtrAlias FHEADER     , HEADER     , write      }
#{setFunPtrAlias FDEBUG      , DEBUG      , debug      }
#{setFunPtrAlias FSSLCTX     , SSL_CTX_   , ssl_ctx    }
#{setFunPtrAlias FIOCTL      , IOCTL      , ioctl      }
#{setFunPtrAlias FCONVFROM   , CONV_FROM_NETWORK_, conv}
#{setFunPtrAlias FCONVTO     , CONV_TO_NETWORK_  , conv}
#{setFunPtrAlias FCONVUTF8   , CONV_FROM_UTF8_   , conv}
#{setFunPtrAlias FSOCKOPT    , SOCKOPT    , sockopt    }
#{setFunPtrAlias FOPENSOCKET , OPENSOCKET , opensocket }
#{setFunPtrAlias FSEEK       , SEEK       , seek       }
#{setFunPtrAlias FSSHKEY     , SSH_KEY    , sshkey     }
#{setFunPtrAlias FINTERLEAVE , INTERLEAVE , write      }
#{setFunPtrAlias FCHUNKBGN   , CHUNK_BGN_ , chunk_bgn  } |7210:----|
#{setFunPtrAlias FCHUNKEND   , CHUNK_END_ , chunk_end  } |7210:----|
#{setFunPtrAlias FFNMATCH    , FNMATCH_   , fnmatch    } |7210:----|
#{setFunPtrAlias FCLOSESOCKET, CLOSESOCKET, closesocket} |7217:----|


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_perform"
  ccurl_easy_perform
    :: Ptr CCURL
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_cleanup"
  ccurl_easy_cleanup
    :: Ptr CCURL
    -> IO ()


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CString
    :: Ptr CCURL
    -> CCURLinfo'CString
    -> Ptr (Ptr CChar)
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CDouble
    :: Ptr CCURL
    -> CCURLinfo'CDouble
    -> Ptr CDouble
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CLong
    :: Ptr CCURL
    -> CCURLinfo'CLong
    -> Ptr CLong
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'SList
    :: Ptr CCURL
    -> CCURLinfo'SList
    -> Ptr (Ptr CCURL_slist)
    -> IO CCURLcode

foreign import ccall "curl_easy_getinfo"
  ccurl_easy_getinfo'CertI
    :: Ptr CCURL
    -> CCURLinfo'CertI
    -> Ptr (Ptr CCURL_certinfo)
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_duphandle"
  ccurl_easy_duphandle
    :: Ptr CCURL
    -> IO (Ptr CCURL)


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_reset"
  ccurl_easy_reset
    :: Ptr CCURL
    -> IO ()


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_recv"
  ccurl_easy_recv
    :: Ptr CCURL
    -> Ptr a
    -> CSize
    -> Ptr CSize
    -> IO CCURLcode


-------------------------------------------------------------------------------
foreign import ccall "curl_easy_send"
  ccurl_easy_send
    :: Ptr CCURL
    -> Ptr a
    -> CSize
    -> Ptr CSize
    -> IO CCURLcode

 
-------------------------------------------------------------------------------
-- * Definitions from \"curl/multi.h\"
-------------------------------------------------------------------------------
-- todo

