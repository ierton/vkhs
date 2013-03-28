-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Types
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Network.Curlhs.Types where

import Foreign.Ptr       (Ptr, FunPtr)

import Data.ByteString   (ByteString)
import Data.Typeable     (Typeable)
import Data.IORef        (IORef)
import Data.Time         (UTCTime)
import Data.Int          (Int64)

import Control.Exception (Exception)

import Network.Curlhs.Base


-------------------------------------------------------------------------------
data CURL = CURL
  { ccurlptr :: Ptr CCURL
  , cb_write :: IORef (Maybe (FunPtr CCURL_write_callback))
  , cb_read  :: IORef (Maybe (FunPtr CCURL_read_callback ))
  }


-------------------------------------------------------------------------------
data CURLcode
  = CURLE_OK
  | CURLE_UNSUPPORTED_PROTOCOL
  | CURLE_FAILED_INIT
  | CURLE_URL_MALFORMAT
  | CURLE_NOT_BUILT_IN             |7215:----|
  | CURLE_COULDNT_RESOLVE_PROXY
  | CURLE_COULDNT_RESOLVE_HOST
  | CURLE_COULDNT_CONNECT
  | CURLE_FTP_WEIRD_SERVER_REPLY
  | CURLE_REMOTE_ACCESS_DENIED
  | CURLE_FTP_ACCEPT_FAILED        |7240:----|
  | CURLE_FTP_WEIRD_PASS_REPLY
  | CURLE_FTP_ACCEPT_TIMEOUT       |7240:----|
  | CURLE_FTP_WEIRD_PASV_REPLY
  | CURLE_FTP_WEIRD_227_FORMAT
  | CURLE_FTP_CANT_GET_HOST
  | CURLE_FTP_COULDNT_SET_TYPE
  | CURLE_PARTIAL_FILE
  | CURLE_FTP_COULDNT_RETR_FILE
  | CURLE_QUOTE_ERROR
  | CURLE_HTTP_RETURNED_ERROR
  | CURLE_WRITE_ERROR
  | CURLE_UPLOAD_FAILED
  | CURLE_READ_ERROR
  | CURLE_OUT_OF_MEMORY
  | CURLE_OPERATION_TIMEDOUT
  | CURLE_FTP_PORT_FAILED
  | CURLE_FTP_COULDNT_USE_REST
  | CURLE_RANGE_ERROR
  | CURLE_HTTP_POST_ERROR
  | CURLE_SSL_CONNECT_ERROR
  | CURLE_BAD_DOWNLOAD_RESUME
  | CURLE_FILE_COULDNT_READ_FILE
  | CURLE_LDAP_CANNOT_BIND
  | CURLE_LDAP_SEARCH_FAILED
  | CURLE_FUNCTION_NOT_FOUND
  | CURLE_ABORTED_BY_CALLBACK
  | CURLE_BAD_FUNCTION_ARGUMENT
  | CURLE_INTERFACE_FAILED
  | CURLE_TOO_MANY_REDIRECTS
  | CURLE_UNKNOWN_TELNET_OPTION    |----:7214|
  | CURLE_UNKNOWN_OPTION           |7215:----|
  | CURLE_TELNET_OPTION_SYNTAX
  | CURLE_PEER_FAILED_VERIFICATION
  | CURLE_GOT_NOTHING
  | CURLE_SSL_ENGINE_NOTFOUND
  | CURLE_SSL_ENGINE_SETFAILED
  | CURLE_SEND_ERROR
  | CURLE_RECV_ERROR
  | CURLE_SSL_CERTPROBLEM
  | CURLE_SSL_CIPHER
  | CURLE_SSL_CACERT
  | CURLE_BAD_CONTENT_ENCODING
  | CURLE_LDAP_INVALID_URL
  | CURLE_FILESIZE_EXCEEDED
  | CURLE_USE_SSL_FAILED
  | CURLE_SEND_FAIL_REWIND
  | CURLE_SSL_ENGINE_INITFAILED
  | CURLE_LOGIN_DENIED
  | CURLE_TFTP_NOTFOUND
  | CURLE_TFTP_PERM
  | CURLE_REMOTE_DISK_FULL
  | CURLE_TFTP_ILLEGAL
  | CURLE_TFTP_UNKNOWNID
  | CURLE_REMOTE_FILE_EXISTS
  | CURLE_TFTP_NOSUCHUSER
  | CURLE_CONV_FAILED
  | CURLE_CONV_REQD
  | CURLE_SSL_CACERT_BADFILE
  | CURLE_REMOTE_FILE_NOT_FOUND
  | CURLE_SSH
  | CURLE_SSL_SHUTDOWN_FAILED
  | CURLE_AGAIN
  | CURLE_SSL_CRL_BADFILE
  | CURLE_SSL_ISSUER_ERROR
  | CURLE_FTP_PRET_FAILED
  | CURLE_RTSP_CSEQ_ERROR
  | CURLE_RTSP_SESSION_ERROR
  | CURLE_FTP_BAD_FILE_LIST        |7210:----|
  | CURLE_CHUNK_FAILED             |7210:----|
  deriving (Eq, Show, Typeable)

instance Exception CURLcode


-------------------------------------------------------------------------------
data CURL_version_info_data = CURL_version_info_data
  { curl_version_info_data_version         :: String
  , curl_version_info_data_version_num     :: Int
  , curl_version_info_data_host            :: String
  , curl_version_info_data_features        :: [CURL_version]
  , curl_version_info_data_ssl_version     :: Maybe String
  , curl_version_info_data_ssl_version_num :: Int
  , curl_version_info_data_libz_version    :: Maybe String
  , curl_version_info_data_protocols       :: [String]
  , curl_version_info_data_ares            :: Maybe String
  , curl_version_info_data_ares_num        :: Int
  , curl_version_info_data_libidn          :: Maybe String
  , curl_version_info_data_iconv_ver_num   :: Int
  , curl_version_info_data_libssh_version  :: Maybe String
  } deriving (Show)


-------------------------------------------------------------------------------
data CURL_version
  = CURL_VERSION_IPV6
  | CURL_VERSION_KERBEROS4
  | CURL_VERSION_SSL
  | CURL_VERSION_LIBZ
  | CURL_VERSION_NTLM
  | CURL_VERSION_GSSNEGOTIATE
  | CURL_VERSION_DEBUG
  | CURL_VERSION_ASYNCHDNS
  | CURL_VERSION_SPNEGO
  | CURL_VERSION_LARGEFILE
  | CURL_VERSION_IDN
  | CURL_VERSION_SSPI
  | CURL_VERSION_CONV
  | CURL_VERSION_CURLDEBUG
  | CURL_VERSION_TLSAUTH_SRP
  | CURL_VERSION_NTLM_WB
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLinfo = CURLinfo
  { curlinfo_effective_url           :: String
  , curlinfo_response_code           :: Maybe Int
  , curlinfo_http_connectcode        :: Maybe Int
  , curlinfo_filetime                :: Maybe UTCTime
  , curlinfo_total_time              :: Double
  , curlinfo_namelookup_time         :: Double
  , curlinfo_connect_time            :: Double
  , curlinfo_appconnect_time         :: Double
  , curlinfo_pretransfer_time        :: Double
  , curlinfo_starttransfer_time      :: Double
  , curlinfo_redirect_time           :: Double
  , curlinfo_redirect_count          :: Int
  , curlinfo_redirect_url            :: Maybe String
  , curlinfo_size_upload             :: Double
  , curlinfo_size_download           :: Double
  , curlinfo_speed_download          :: Double
  , curlinfo_speed_upload            :: Double
  , curlinfo_header_size             :: Int
  , curlinfo_request_size            :: Int
  , curlinfo_ssl_verifyresult        :: Int  -- Bool??
  , curlinfo_ssl_engines             :: [String]
  , curlinfo_content_length_download :: Maybe Double
  , curlinfo_content_length_upload   :: Maybe Double
  , curlinfo_content_type            :: Maybe String
--  , curlinfo_private                 :: String   -- void??
  , curlinfo_httpauth_avail          :: [CURLauth]
  , curlinfo_proxyauth_avail         :: [CURLauth]
  , curlinfo_os_errno                :: Int
  , curlinfo_num_connects            :: Int
  , curlinfo_primary_ip              :: String
  , curlinfo_primary_port            :: Int
  , curlinfo_local_ip                :: String
  , curlinfo_local_port              :: Int
  , curlinfo_cookielist              :: [String]
  , curlinfo_lastsocket              :: Maybe Int
  , curlinfo_ftp_entry_path          :: Maybe String
  , curlinfo_certinfo                :: [[String]]
  , curlinfo_condition_unmet         :: Bool
  , curlinfo_rtsp_session_id         :: Maybe String
  , curlinfo_rtsp_client_cseq        :: Int
  , curlinfo_rtsp_server_cseq        :: Int
  , curlinfo_rtsp_cseq_recv          :: Int
  } deriving (Show)



-------------------------------------------------------------------------------
data CURLoption
  ---- BEHAVIOR OPTIONS -------------------------------------------------------
  = CURLOPT_VERBOSE                 Bool
  | CURLOPT_HEADER                  Bool
  | CURLOPT_NOPROGRESS              Bool
  | CURLOPT_NOSIGNAL                Bool
  | CURLOPT_WILDCARDMATCH           Bool |7210:----|

  ---- CALLBACK OPTIONS -------------------------------------------------------
  | CURLOPT_WRITEFUNCTION           (Maybe CURL_write_callback)
 -- CURLOPT_WRITEDATA
  | CURLOPT_READFUNCTION            (Maybe CURL_read_callback)
 -- CURLOPT_READDATA
 -- CURLOPT_IOCTLFUNCTION
 -- CURLOPT_IOCTLDATA
 -- CURLOPT_SEEKFUNCTION
 -- CURLOPT_SEEKDATA
 -- CURLOPT_SOCKOPTFUNCTION
 -- CURLOPT_SOCKOPTDATA
 -- CURLOPT_OPENSOCKETFUNCTION
 -- CURLOPT_OPENSOCKETDATA
 -- CURLOPT_CLOSESOCKETFUNCTION |7217:----|
 -- CURLOPT_CLOSESOCKETDATA     |7217:----|
 -- CURLOPT_PROGRESSFUNCTION
 -- CURLOPT_PROGRESSDATA
 -- CURLOPT_HEADERFUNCTION
 -- CURLOPT_HEADERDATA
 -- CURLOPT_DEBUGFUNCTION
 -- CURLOPT_DEBUGDATA
 -- CURLOPT_SSL_CTX_FUNCTION
 -- CURLOPT_SSL_CTX_DATA
 -- CURLOPT_CONV_TO_NETWORK_FUNCTION
 -- CURLOPT_CONV_FROM_NETWORK_FUNCTION
 -- CURLOPT_CONV_FROM_UTF8_FUNCTION
 -- CURLOPT_INTERLEAVEFUNCTION
 -- CURLOPT_INTERLEAVEDATA
 -- CURLOPT_CHUNK_BGN_FUNCTION |7210:----|
 -- CURLOPT_CHUNK_END_FUNCTION |7210:----|
 -- CURLOPT_CHUNK_DATA         |7210:----|
 -- CURLOPT_FNMATCH_FUNCTION   |7210:----|
 -- CURLOPT_FNMATCH_DATA       |7210:----|

  ---- ERROR OPTIONS ----------------------------------------------------------
 -- CURLOPT_ERRORBUFFER             (IORef (Maybe ByteString))
 -- CURLOPT_STDERR                  Handle
  | CURLOPT_FAILONERROR             Bool

  ---- NETWORK OPTIONS --------------------------------------------------------
  | CURLOPT_URL                     ByteString
  | CURLOPT_PROTOCOLS               [CURLproto]
  | CURLOPT_REDIR_PROTOCOLS         [CURLproto]
  | CURLOPT_PROXY                   ByteString
  | CURLOPT_PROXYPORT               Int
  | CURLOPT_PROXYTYPE               CURLproxy
  | CURLOPT_NOPROXY                 ByteString
  | CURLOPT_HTTPPROXYTUNNEL         Bool
  | CURLOPT_SOCKS5_GSSAPI_SERVICE   ByteString
  | CURLOPT_SOCKS5_GSSAPI_NEC       Bool
  | CURLOPT_INTERFACE               ByteString
  | CURLOPT_LOCALPORT               Int
  | CURLOPT_LOCALPORTRANGE          Int
  | CURLOPT_DNS_CACHE_TIMEOUT       Int
  | CURLOPT_DNS_USE_GLOBAL_CACHE    Bool
  | CURLOPT_BUFFERSIZE              Int
  | CURLOPT_PORT                    Int
  | CURLOPT_TCP_NODELAY             Bool
  | CURLOPT_ADDRESS_SCOPE           Int
  | CURLOPT_TCP_KEEPALIVE           Bool          |7250:----|
  | CURLOPT_TCP_KEEPIDLE            Int           |7250:----|
  | CURLOPT_TCP_KEEPINTVL           Int           |7250:----|

  ---- NAMES and PASSWORDS OPTIONS (Authentication) ---------------------------
  | CURLOPT_NETRC                   CURLnetrc
  | CURLOPT_NETRC_FILE              ByteString
  | CURLOPT_USERPWD                 ByteString
  | CURLOPT_PROXYUSERPWD            ByteString
  | CURLOPT_USERNAME                ByteString
  | CURLOPT_PASSWORD                ByteString
  | CURLOPT_PROXYUSERNAME           ByteString
  | CURLOPT_PROXYPASSWORD           ByteString
  | CURLOPT_HTTPAUTH                [CURLauth]
 -- CURLOPT_TLSAUTH_TYPE            [CURLtlsauth] |7214:----|
  | CURLOPT_TLSAUTH_TYPE            ByteString    |7214:----|
  | CURLOPT_TLSAUTH_USERNAME        ByteString    |7214:----|
  | CURLOPT_TLSAUTH_PASSWORD        ByteString    |7214:----|
  | CURLOPT_PROXYAUTH               [CURLauth]

  ---- HTTP OPTIONS -----------------------------------------------------------
  | CURLOPT_AUTOREFERER             Bool
  | CURLOPT_ENCODING                ByteString    |----:7215|
  | CURLOPT_ACCEPT_ENCODING         ByteString    |7216:----|
  | CURLOPT_TRANSFER_ENCODING       Bool          |7216:----|
  | CURLOPT_FOLLOWLOCATION          Bool
  | CURLOPT_UNRESTRICTED_AUTH       Bool
  | CURLOPT_MAXREDIRS               Int
  | CURLOPT_POSTREDIR               [CURLredir]
  | CURLOPT_PUT                     Bool
  | CURLOPT_POST                    Bool
 -- CURLOPT_POSTFIELDS              ByteString -- not copied
  | CURLOPT_POSTFIELDSIZE           Int
  | CURLOPT_POSTFIELDSIZE_LARGE     Int64
  | CURLOPT_COPYPOSTFIELDS          ByteString
 -- CURLOPT_HTTPPOST                [CURL_httppost]
  | CURLOPT_REFERER                 ByteString
  | CURLOPT_USERAGENT               ByteString
 -- CURLOPT_HTTPHEADER              [ByteString]
 -- CURLOPT_HTTP200ALIASES          [ByteString]
  | CURLOPT_COOKIE                  ByteString
  | CURLOPT_COOKIEFILE              ByteString
  | CURLOPT_COOKIEJAR               ByteString
  | CURLOPT_COOKIESESSION           Bool
  | CURLOPT_COOKIELIST              ByteString
  | CURLOPT_HTTPGET                 Bool
  | CURLOPT_HTTP_VERSION            CURLhttpver
  | CURLOPT_IGNORE_CONTENT_LENGTH   Bool
  | CURLOPT_HTTP_CONTENT_DECODING   Bool
  | CURLOPT_HTTP_TRANSFER_DECODING  Bool

  ---- SMTP OPTIONS -----------------------------------------------------------
  | CURLOPT_MAIL_FROM               ByteString
 -- CURLOPT_MAIL_RCTP               [ByteString]
  | CURLOPT_MAIL_AUTH               ByteString    |7250:----|

  ---- TFTP OPTIONS -----------------------------------------------------------
  | CURLOPT_TFTP_BLKSIZE            Int

  ---- FTP OPTIONS ------------------------------------------------------------
  | CURLOPT_FTPPORT                 ByteString
 -- CURLOPT_QUOTE                   [ByteString]
 -- CURLOPT_POSTQUOTE               [ByteString]
 -- CURLOPT_PREQUOTE                [ByteString]
  | CURLOPT_DIRLISTONLY             Bool
  | CURLOPT_APPEND                  Bool
  | CURLOPT_FTP_USE_EPRT            Bool
  | CURLOPT_FTP_USE_EPSV            Bool
  | CURLOPT_FTP_USE_PRET            Bool
  | CURLOPT_FTP_CREATE_MISSING_DIRS CURLftpcreate
  | CURLOPT_FTP_RESPONSE_TIMEOUT    Int
  | CURLOPT_FTP_ALTERNATIVE_TO_USER ByteString
  | CURLOPT_FTP_SKIP_PASV_IP        Bool
  | CURLOPT_FTPSSLAUTH              CURLftpauth
  | CURLOPT_FTP_SSL_CCC             CURLftpssl
  | CURLOPT_FTP_ACCOUNT             ByteString
  | CURLOPT_FTP_FILEMETHOD          CURLftpmethod

  ---- RTSP OPTIONS -----------------------------------------------------------
  | CURLOPT_RTSP_REQUEST            CURLrtspreq
  | CURLOPT_RTSP_SESSION_ID         ByteString
  | CURLOPT_RTSP_STREAM_URI         ByteString
  | CURLOPT_RTSP_TRANSPORT          ByteString
 -- CURLOPT_RTSP_HEADER             [ByteString]
  | CURLOPT_RTSP_CLIENT_CSEQ        Int
  | CURLOPT_RTSP_SERVER_CSEQ        Int

  ---- PROTOCOL OPTIONS -------------------------------------------------------
  | CURLOPT_TRANSFERTEXT            Bool
  | CURLOPT_PROXY_TRANSFER_MODE     Bool
  | CURLOPT_CRLF                    Bool
  | CURLOPT_RANGE                   ByteString
  | CURLOPT_RESUME_FROM             Int
  | CURLOPT_RESUME_FROM_LARGE       Int64
  | CURLOPT_CUSTOMREQUEST           ByteString
  | CURLOPT_FILETIME                Bool
  | CURLOPT_NOBODY                  Bool
  | CURLOPT_INFILESIZE              Int
  | CURLOPT_INFILESIZE_LARGE        Int64
  | CURLOPT_UPLOAD                  Bool
  | CURLOPT_MAXFILESIZE             Int
  | CURLOPT_MAXFILESIZE_LARGE       Int64
  | CURLOPT_TIMECONDITION           CURLtimecond
  | CURLOPT_TIMEVALUE               UTCTime

  ---- CONNECTION OPTIONS -----------------------------------------------------
  | CURLOPT_TIMEOUT                 Int
  | CURLOPT_TIMEOUT_MS              Int
  | CURLOPT_LOW_SPEED_LIMIT         Int
  | CURLOPT_LOW_SPEED_TIME          Int
  | CURLOPT_MAX_SEND_SPEED_LARGE    Int64
  | CURLOPT_MAX_RECV_SPEED_LARGE    Int64
  | CURLOPT_MAXCONNECTS             Int
  | CURLOPT_CLOSEPOLICY             CURLclosepol
  | CURLOPT_FRESH_CONNECT           Bool
  | CURLOPT_FORBID_REUSE            Bool
  | CURLOPT_CONNECTTIMEOUT          Int
  | CURLOPT_CONNECTTIMEOUT_MS       Int
  | CURLOPT_IPRESOLVE               CURLipresolve
  | CURLOPT_CONNECT_ONLY            Bool
  | CURLOPT_USE_SSL                 CURLusessl
 -- CURLOPT_RESOLVE                 [ByteString]  |7213:----|
  | CURLOPT_DNS_SERVERS             ByteString    |7240:----|
  | CURLOPT_ACCEPTTIMEOUT_MS        Int           |7240:----|

  ---- SSL and SECURITY OPTIONS -----------------------------------------------
  | CURLOPT_SSLCERT                 ByteString
  | CURLOPT_SSLCERTTYPE             ByteString
  | CURLOPT_SSLKEY                  ByteString
  | CURLOPT_SSLKEYTYPE              ByteString
  | CURLOPT_KEYPASSWD               ByteString
  | CURLOPT_SSLENGINE               ByteString
  | CURLOPT_SSLENGINE_DEFAULT       Bool
  | CURLOPT_SSLVERSION              CURLsslver
  | CURLOPT_SSL_VERIFYPEER          Bool
  | CURLOPT_CAINFO                  ByteString
  | CURLOPT_ISSUERCERT              ByteString
  | CURLOPT_CAPATH                  ByteString
  | CURLOPT_CRLFILE                 ByteString
  | CURLOPT_SSL_VERIFYHOST          Int
  | CURLOPT_CERTINFO                Bool
  | CURLOPT_RANDOM_FILE             ByteString
  | CURLOPT_EGDSOCKET               ByteString
  | CURLOPT_SSL_CIPHER_LIST         ByteString
  | CURLOPT_SSL_SESSIONID_CACHE     Bool
  | CURLOPT_SSL_OPTIONS             CURLsslopt    |7250:----|
  | CURLOPT_KRBLEVEL                ByteString
  | CURLOPT_GSSAPI_DELEGATION       CURLgssapi    |7220:----|

  ---- SSH OPTIONS ------------------------------------------------------------
  | CURLOPT_SSH_AUTH_TYPES          [CURLsshauth]
  | CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 ByteString
  | CURLOPT_SSH_PUBLIC_KEYFILE      ByteString
  | CURLOPT_SSH_PRIVATE_KEYFILE     ByteString
  | CURLOPT_SSH_KNOWNHOSTS          ByteString
 -- CURLOPT_SSH_KEYFUNCTION         (Maybe CURL_sshkey_callback)
 -- CURLOPT_SSH_KEYDATA             -- ?

  ---- OTHER OPTIONS ----------------------------------------------------------
 -- CURLOPT_PRIVATE                 -- ?
 -- CURLOPT_SHARE                   CURLSH
  | CURLOPT_NEW_FILE_PERMS          Int
  | CURLOPT_NEW_DIRECTORY_PERMS     Int

  ---- TELNET OPTIONS ---------------------------------------------------------
 -- CURLOPT_TELNETOPTIONS           [ByteString]


-------------------------------------------------------------------------------
data CURLproto
  = CURLPROTO_ALL
  | CURLPROTO_HTTP
  | CURLPROTO_HTTPS
  | CURLPROTO_FTP
  | CURLPROTO_FTPS
  | CURLPROTO_SCP
  | CURLPROTO_SFTP
  | CURLPROTO_TELNET
  | CURLPROTO_LDAP
  | CURLPROTO_LDAPS
  | CURLPROTO_DICT
  | CURLPROTO_FILE
  | CURLPROTO_TFTP
  | CURLPROTO_IMAP
  | CURLPROTO_IMAPS
  | CURLPROTO_POP3
  | CURLPROTO_POP3S
  | CURLPROTO_SMTP
  | CURLPROTO_SMTPS
  | CURLPROTO_RTSP
  | CURLPROTO_RTMP   |7210:----|
  | CURLPROTO_RTMPT  |7210:----|
  | CURLPROTO_RTMPE  |7210:----|
  | CURLPROTO_RTMPTE |7210:----|
  | CURLPROTO_RTMPS  |7210:----|
  | CURLPROTO_RTMPTS |7210:----|
  | CURLPROTO_GOPHER |7212:----|
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLproxy
  = CURLPROXY_HTTP
  | CURLPROXY_HTTP_1_0
  | CURLPROXY_SOCKS4
  | CURLPROXY_SOCKS5
  | CURLPROXY_SOCKS4A
  | CURLPROXY_SOCKS5_HOSTNAME
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLnetrc
  = CURL_NETRC_IGNORED
  | CURL_NETRC_OPTIONAL
  | CURL_NETRC_REQUIRED
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLauth
  = CURLAUTH_BASIC
  | CURLAUTH_DIGEST
  | CURLAUTH_DIGEST_IE
  | CURLAUTH_GSSNEGOTIATE
  | CURLAUTH_NTLM
  | CURLAUTH_NTLM_WB      |7220:----|
  | CURLAUTH_ONLY         |7213:----|
  | CURLAUTH_ANY
  | CURLAUTH_ANYSAFE
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLtlsauth                                                 |7214:----|
  = CURL_TLSAUTH_SRP                                             |7214:----|
  deriving (Eq, Show)                                            |7214:----|


-------------------------------------------------------------------------------
data CURLredir
  = CURL_REDIR_GET_ALL
  | CURL_REDIR_POST_301
  | CURL_REDIR_POST_302
  | CURL_REDIR_POST_ALL
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLhttpver
  = CURL_HTTP_VERSION_NONE
  | CURL_HTTP_VERSION_1_0
  | CURL_HTTP_VERSION_1_1
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLftpcreate
  = CURLFTP_CREATE_DIR_NONE
  | CURLFTP_CREATE_DIR
  | CURLFTP_CREATE_DIR_RETRY
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLftpauth
  = CURLFTPAUTH_DEFAULT
  | CURLFTPAUTH_SSL
  | CURLFTPAUTH_TLS
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLftpssl
  = CURLFTPSSL_CCC_NONE
  | CURLFTPSSL_CCC_PASSIVE
  | CURLFTPSSL_CCC_ACTIVE
  deriving (Eq, Show)
 

-------------------------------------------------------------------------------
data CURLftpmethod
  = CURLFTPMETHOD_DEFAULT
  | CURLFTPMETHOD_MULTICWD
  | CURLFTPMETHOD_NOCWD
  | CURLFTPMETHOD_SINGLECWD
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLrtspreq
  = CURL_RTSPREQ_OPTIONS
  | CURL_RTSPREQ_DESCRIBE
  | CURL_RTSPREQ_ANNOUNCE
  | CURL_RTSPREQ_SETUP
  | CURL_RTSPREQ_PLAY
  | CURL_RTSPREQ_PAUSE
  | CURL_RTSPREQ_TEARDOWN
  | CURL_RTSPREQ_GET_PARAMETER
  | CURL_RTSPREQ_SET_PARAMETER
  | CURL_RTSPREQ_RECORD
  | CURL_RTSPREQ_RECEIVE
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLtimecond
  = CURL_TIMECOND_NONE
  | CURL_TIMECOND_IFMODSINCE
  | CURL_TIMECOND_IFUNMODSINCE
  | CURL_TIMECOND_LASTMOD
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLclosepol
  = CURLCLOSEPOLICY_NONE
  | CURLCLOSEPOLICY_OLDEST
  | CURLCLOSEPOLICY_LEAST_RECENTLY_USED
  | CURLCLOSEPOLICY_LEAST_TRAFFIC
  | CURLCLOSEPOLICY_SLOWEST
  | CURLCLOSEPOLICY_CALLBACK
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLipresolve
  = CURL_IPRESOLVE_WHATEVER
  | CURL_IPRESOLVE_V4
  | CURL_IPRESOLVE_V6
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLusessl
  = CURLUSESSL_NONE
  | CURLUSESSL_TRY
  | CURLUSESSL_CONTROL
  | CURLUSESSL_ALL
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLsslver
  = CURL_SSLVERSION_DEFAULT
  | CURL_SSLVERSION_TLSv1
  | CURL_SSLVERSION_SSLv2
  | CURL_SSLVERSION_SSLv3
  deriving (Eq, Show)


-------------------------------------------------------------------------------
data CURLsslopt                                                  |7250:----|
  = CURLSSLOPT_ALLOW_BEAST                                       |7250:----|
  deriving (Eq, Show)                                            |7250:----|


-------------------------------------------------------------------------------
data CURLgssapi                                                  |7220:----|
  = CURLGSSAPI_DELEGATION_NONE                                   |7220:----|
  | CURLGSSAPI_DELEGATION_POLICY_FLAG                            |7220:----|
  | CURLGSSAPI_DELEGATION_FLAG                                   |7220:----|
  deriving (Eq, Show)                                            |7220:----|


-------------------------------------------------------------------------------
data CURLsshauth
  = CURLSSH_AUTH_ANY
  | CURLSSH_AUTH_NONE
  | CURLSSH_AUTH_PUBLICKEY
  | CURLSSH_AUTH_PASSWORD
  | CURLSSH_AUTH_HOST
  | CURLSSH_AUTH_KEYBOARD
  | CURLSSH_AUTH_DEFAULT
  deriving (Eq, Show)


-------------------------------------------------------------------------------
type CURL_write_callback = ByteString -> IO CURL_write_response

data CURL_write_response
  = CURL_WRITEFUNC_OK
  | CURL_WRITEFUNC_FAIL
  | CURL_WRITEFUNC_PAUSE
  deriving (Eq)


type CURL_read_callback = Int -> IO CURL_read_response

data CURL_read_response
  = CURL_READFUNC_OK ByteString
  | CURL_READFUNC_ABORT
  | CURL_READFUNC_PAUSE
  deriving (Eq)


