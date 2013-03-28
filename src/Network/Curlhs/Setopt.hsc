-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Setopt
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Setopt
  ( curl_easy_setopt
  , freeCallbacks
  ) where

import Foreign.Marshal.Utils (copyBytes, fromBool)
import Foreign.C.Types       (CLong)
import Foreign.Ptr           (FunPtr, nullFunPtr, freeHaskellFunPtr)

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Clock       (UTCTime)
import Data.Bits             ((.|.))
import Data.List             (foldl')
import Data.IORef            (IORef, atomicModifyIORef)

import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString        (ByteString, useAsCString, packCStringLen)

import Network.Curlhs.Errors
import Network.Curlhs.Types
import Network.Curlhs.Base


-------------------------------------------------------------------------------
freeCallbacks :: CURL -> IO ()
freeCallbacks curl = do
  keepCallback (cb_write curl) Nothing
  keepCallback (cb_read  curl) Nothing

keepCallback :: IORef (Maybe (FunPtr a)) -> Maybe (FunPtr a) -> IO ()
keepCallback r mf =
  atomicModifyIORef r (\v -> (mf, v)) >>= maybe (return ()) freeHaskellFunPtr

makeCallback :: Maybe cb -> IORef (Maybe (FunPtr a))
             -> (FunPtr a -> IO CCURLcode) -> (cb -> IO (FunPtr a)) -> IO ()
makeCallback (Just cb) ref setcb wrapcb = withCODE $ do
  fptr <- wrapcb cb
  code <- setcb fptr
  if (code == cCURLE_OK)
    then keepCallback ref (Just fptr)
    else freeHaskellFunPtr fptr
  return code
makeCallback Nothing ref setcb _ = withCODE $ do
  code <- setcb nullFunPtr
  keepCallback ref Nothing
  return code


-------------------------------------------------------------------------------
#{let setopt opt, fun = #opt " x -> " #fun " c" #opt " x"}

-------------------------------------------------------------------------------
-- | Set options for a curl easy handle
--   (<http://curl.haxx.se/libcurl/c/curl_easy_setopt.html>).
-------------------------------------------------------------------------------
curl_easy_setopt :: CURL -> [CURLoption] -> IO ()
curl_easy_setopt curl opts = flip mapM_ opts $ \opt -> case opt of

  ---- CALLBACK OPTIONS -------------------------------------------------------
  CURLOPT_WRITEFUNCTION f -> so'FWRITE curl f
  CURLOPT_READFUNCTION  f -> so'FREAD  curl f

  ---- BEHAVIOR OPTIONS -------------------------------------------------------
  #{setopt CURLOPT_VERBOSE                , bool     }
  #{setopt CURLOPT_HEADER                 , bool     }
  #{setopt CURLOPT_NOPROGRESS             , bool     }
  #{setopt CURLOPT_NOSIGNAL               , bool     }
  #{setopt CURLOPT_WILDCARDMATCH          , bool     } |7210:----|

  ---- ERROR OPTIONS ----------------------------------------------------------
  -- CURLOPT_ERRORBUFFER
  -- CURLOPT_STDERR
  #{setopt CURLOPT_FAILONERROR            , bool     }

  ---- NETWORK OPTIONS --------------------------------------------------------
  #{setopt CURLOPT_URL                    , string   }
  #{setopt CURLOPT_PROTOCOLS              , enum     }
  #{setopt CURLOPT_REDIR_PROTOCOLS        , enum     }
  #{setopt CURLOPT_PROXY                  , string   }
  #{setopt CURLOPT_PROXYPORT              , int      }
  #{setopt CURLOPT_PROXYTYPE              , enum     }
  #{setopt CURLOPT_NOPROXY                , string   }
  #{setopt CURLOPT_HTTPPROXYTUNNEL        , bool     }
  #{setopt CURLOPT_SOCKS5_GSSAPI_SERVICE  , string   }
  #{setopt CURLOPT_SOCKS5_GSSAPI_NEC      , bool     }
  #{setopt CURLOPT_INTERFACE              , string   }
  #{setopt CURLOPT_LOCALPORT              , int      }
  #{setopt CURLOPT_LOCALPORTRANGE         , int      }
  #{setopt CURLOPT_DNS_CACHE_TIMEOUT      , int      }
  #{setopt CURLOPT_DNS_USE_GLOBAL_CACHE   , bool     }
  #{setopt CURLOPT_BUFFERSIZE             , int      }
  #{setopt CURLOPT_PORT                   , int      }
  #{setopt CURLOPT_TCP_NODELAY            , bool     }
  #{setopt CURLOPT_ADDRESS_SCOPE          , int      }
  #{setopt CURLOPT_TCP_KEEPALIVE          , bool     } |7250:----|
  #{setopt CURLOPT_TCP_KEEPIDLE           , int      } |7250:----|
  #{setopt CURLOPT_TCP_KEEPINTVL          , int      } |7250:----|

  ---- NAMES and PASSWORDS OPTIONS (Authentication) ---------------------------
  #{setopt CURLOPT_NETRC                  , enum     }
  #{setopt CURLOPT_NETRC_FILE             , string   }
  #{setopt CURLOPT_USERPWD                , string   }
  #{setopt CURLOPT_PROXYUSERPWD           , string   }
  #{setopt CURLOPT_USERNAME               , string   }
  #{setopt CURLOPT_PASSWORD               , string   }
  #{setopt CURLOPT_PROXYUSERNAME          , string   }
  #{setopt CURLOPT_PROXYPASSWORD          , string   }
  #{setopt CURLOPT_HTTPAUTH               , enum     }
  #{setopt CURLOPT_TLSAUTH_TYPE           , string   } |7214:----|
  #{setopt CURLOPT_TLSAUTH_USERNAME       , string   } |7214:----|
  #{setopt CURLOPT_TLSAUTH_PASSWORD       , string   } |7214:----|
  #{setopt CURLOPT_PROXYAUTH              , enum     }

  ---- HTTP OPTIONS -----------------------------------------------------------
  #{setopt CURLOPT_AUTOREFERER            , bool     }
  #{setopt CURLOPT_ENCODING               , string   } |----:7215|
  #{setopt CURLOPT_ACCEPT_ENCODING        , string   } |7216:----|
  #{setopt CURLOPT_TRANSFER_ENCODING      , bool     } |7216:----|
  #{setopt CURLOPT_FOLLOWLOCATION         , bool     }
  #{setopt CURLOPT_UNRESTRICTED_AUTH      , bool     }
  #{setopt CURLOPT_MAXREDIRS              , int      }
  #{setopt CURLOPT_POSTREDIR              , enum     }
  #{setopt CURLOPT_PUT                    , bool     }
  #{setopt CURLOPT_POST                   , bool     }
  -- #{setopt CURLOPT_POSTFIELDS             , buffer   }
  #{setopt CURLOPT_POSTFIELDSIZE          , int      }
  #{setopt CURLOPT_POSTFIELDSIZE_LARGE    , int64    }
  #{setopt CURLOPT_COPYPOSTFIELDS         , string   }
  -- CURLOPT_HTTPPOST
  #{setopt CURLOPT_REFERER                , string   }
  #{setopt CURLOPT_USERAGENT              , string   }
  -- #{setopt CURLOPT_HTTPHEADER             , slist    }
  -- #{setopt CURLOPT_HTTP200ALIASES         , slist    }
  #{setopt CURLOPT_COOKIE                 , string   }
  #{setopt CURLOPT_COOKIEFILE             , string   }
  #{setopt CURLOPT_COOKIEJAR              , string   }
  #{setopt CURLOPT_COOKIESESSION          , bool     }
  #{setopt CURLOPT_COOKIELIST             , string   }
  #{setopt CURLOPT_HTTPGET                , bool     }
  #{setopt CURLOPT_HTTP_VERSION           , enum     }
  #{setopt CURLOPT_IGNORE_CONTENT_LENGTH  , bool     }
  #{setopt CURLOPT_HTTP_CONTENT_DECODING  , bool     }
  #{setopt CURLOPT_HTTP_TRANSFER_DECODING , bool     }

  ---- SMTP OPTIONS -----------------------------------------------------------
  #{setopt CURLOPT_MAIL_FROM              , string   }
  -- #{setopt CURLOPT_MAIL_RCTP              , slist    }
  #{setopt CURLOPT_MAIL_AUTH              , string   } |7250:----|

  ---- TFTP OPTIONS -----------------------------------------------------------
  #{setopt CURLOPT_TFTP_BLKSIZE           , int      }

  ---- FTP OPTIONS ------------------------------------------------------------
  #{setopt CURLOPT_FTPPORT                , string   }
  -- #{setopt CURLOPT_QUOTE                  , slist    }
  -- #{setopt CURLOPT_POSTQUOTE              , slist    }
  -- #{setopt CURLOPT_PREQUOTE               , slist    }
  #{setopt CURLOPT_DIRLISTONLY            , bool     }
  #{setopt CURLOPT_APPEND                 , bool     }
  #{setopt CURLOPT_FTP_USE_EPRT           , bool     }
  #{setopt CURLOPT_FTP_USE_EPSV           , bool     }
  #{setopt CURLOPT_FTP_USE_PRET           , bool     }
  #{setopt CURLOPT_FTP_CREATE_MISSING_DIRS, enum     }
  #{setopt CURLOPT_FTP_RESPONSE_TIMEOUT   , int      }
  #{setopt CURLOPT_FTP_ALTERNATIVE_TO_USER, string   }
  #{setopt CURLOPT_FTP_SKIP_PASV_IP       , bool     }
  #{setopt CURLOPT_FTPSSLAUTH             , enum     }
  #{setopt CURLOPT_FTP_SSL_CCC            , enum     }
  #{setopt CURLOPT_FTP_ACCOUNT            , string   }
  #{setopt CURLOPT_FTP_FILEMETHOD         , enum     }

  ---- RTSP OPTIONS -----------------------------------------------------------
  #{setopt CURLOPT_RTSP_REQUEST           , enum     }
  #{setopt CURLOPT_RTSP_SESSION_ID        , string   }
  #{setopt CURLOPT_RTSP_STREAM_URI        , string   }
  #{setopt CURLOPT_RTSP_TRANSPORT         , string   }
  -- #{setopt CURLOPT_RTSP_HEADER            , slist    }
  #{setopt CURLOPT_RTSP_CLIENT_CSEQ       , int      }
  #{setopt CURLOPT_RTSP_SERVER_CSEQ       , int      }

  ---- PROTOCOL OPTIONS -------------------------------------------------------
  #{setopt CURLOPT_TRANSFERTEXT           , bool     }
  #{setopt CURLOPT_PROXY_TRANSFER_MODE    , bool     }
  #{setopt CURLOPT_CRLF                   , bool     }
  #{setopt CURLOPT_RANGE                  , string   }
  #{setopt CURLOPT_RESUME_FROM            , int      }
  #{setopt CURLOPT_RESUME_FROM_LARGE      , int64    }
  #{setopt CURLOPT_CUSTOMREQUEST          , string   }
  #{setopt CURLOPT_FILETIME               , bool     }
  #{setopt CURLOPT_NOBODY                 , bool     }
  #{setopt CURLOPT_INFILESIZE             , int      }
  #{setopt CURLOPT_INFILESIZE_LARGE       , int64    }
  #{setopt CURLOPT_UPLOAD                 , bool     }
  #{setopt CURLOPT_MAXFILESIZE            , int      }
  #{setopt CURLOPT_MAXFILESIZE_LARGE      , int64    }
  #{setopt CURLOPT_TIMECONDITION          , enum     }
  #{setopt CURLOPT_TIMEVALUE              , time     }

  ---- CONNECTION OPTIONS -----------------------------------------------------
  #{setopt CURLOPT_TIMEOUT                , int      }
  #{setopt CURLOPT_TIMEOUT_MS             , int      }
  #{setopt CURLOPT_LOW_SPEED_LIMIT        , int      }
  #{setopt CURLOPT_LOW_SPEED_TIME         , int      }
  #{setopt CURLOPT_MAX_SEND_SPEED_LARGE   , int64    }
  #{setopt CURLOPT_MAX_RECV_SPEED_LARGE   , int64    }
  #{setopt CURLOPT_MAXCONNECTS            , int      }
  #{setopt CURLOPT_CLOSEPOLICY            , enum     }
  #{setopt CURLOPT_FRESH_CONNECT          , bool     }
  #{setopt CURLOPT_FORBID_REUSE           , bool     }
  #{setopt CURLOPT_CONNECTTIMEOUT         , int      }
  #{setopt CURLOPT_CONNECTTIMEOUT_MS      , int      }
  #{setopt CURLOPT_IPRESOLVE              , enum     }
  #{setopt CURLOPT_CONNECT_ONLY           , bool     }
  #{setopt CURLOPT_USE_SSL                , enum     }
  -- #{setopt CURLOPT_RESOLVE                , slist    } |7213:----|
  #{setopt CURLOPT_DNS_SERVERS            , string   } |7240:----|
  #{setopt CURLOPT_ACCEPTTIMEOUT_MS       , int      } |7240:----|

  ---- SSL and SECURITY OPTIONS -----------------------------------------------
  #{setopt CURLOPT_SSLCERT                , string   }
  #{setopt CURLOPT_SSLCERTTYPE            , string   }
  #{setopt CURLOPT_SSLKEY                 , string   }
  #{setopt CURLOPT_SSLKEYTYPE             , string   }
  #{setopt CURLOPT_KEYPASSWD              , string   }
  #{setopt CURLOPT_SSLENGINE              , string   }
  #{setopt CURLOPT_SSLENGINE_DEFAULT      , bool     }
  #{setopt CURLOPT_SSLVERSION             , enum     }
  #{setopt CURLOPT_SSL_VERIFYPEER         , bool     }
  #{setopt CURLOPT_CAINFO                 , string   }
  #{setopt CURLOPT_ISSUERCERT             , string   }
  #{setopt CURLOPT_CAPATH                 , string   }
  #{setopt CURLOPT_CRLFILE                , string   }
  #{setopt CURLOPT_SSL_VERIFYHOST         , int      }
  #{setopt CURLOPT_CERTINFO               , bool     }
  #{setopt CURLOPT_RANDOM_FILE            , string   }
  #{setopt CURLOPT_EGDSOCKET              , string   }
  #{setopt CURLOPT_SSL_CIPHER_LIST        , string   }
  #{setopt CURLOPT_SSL_SESSIONID_CACHE    , bool     }
  #{setopt CURLOPT_SSL_OPTIONS            , enum     } |7250:----|
  #{setopt CURLOPT_KRBLEVEL               , string   }
  #{setopt CURLOPT_GSSAPI_DELEGATION      , enum     } |7220:----|

  ---- SSH OPTIONS ------------------------------------------------------------
  #{setopt CURLOPT_SSH_AUTH_TYPES         , enum     }
  #{setopt CURLOPT_SSH_HOST_PUBLIC_KEY_MD5, string   }
  #{setopt CURLOPT_SSH_PUBLIC_KEYFILE     , string   }
  #{setopt CURLOPT_SSH_PRIVATE_KEYFILE    , string   }
  #{setopt CURLOPT_SSH_KNOWNHOSTS         , string   }
  -- CURLOPT_SSH_KEYFUNCTION
  -- CURLOPT_SSH_KEYDATA

  ---- OTHER OPTIONS ----------------------------------------------------------
  -- CURLOPT_PRIVATE
  -- CURLOPT_SHARE
  #{setopt CURLOPT_NEW_FILE_PERMS         , int      }
  #{setopt CURLOPT_NEW_DIRECTORY_PERMS    , int      }

  ---- TELNET OPTIONS ---------------------------------------------------------
  -- #{setopt CURLOPT_TELNETOPTIONS          , slist    }

  -----------------------------------------------------------------------------
  where
    string copt s = setopt'CString curl copt s
    int64  copt x = setopt'Int64   curl copt (fromIntegral   x)
    int    copt x = setopt'CLong   curl copt (fromIntegral   x)
    bool   copt x = setopt'CLong   curl copt (fromBool       x)
    time   copt x = setopt'CLong   curl copt (fromUTCTime    x)
    enum   copt x = setopt'CLong   curl copt (fromCURLenum   x)



-------------------------------------------------------------------------------
setopt'CString :: CURL -> CCURLoption'CString -> ByteString -> IO ()
setopt'CString curl copt val = useAsCString val $ \ptr ->
  withCODE $ ccurl_easy_setopt'CString (ccurlptr curl) copt ptr

setopt'Int64 :: CURL -> CCURLoption'Int64 -> CCURL_off_t -> IO ()
setopt'Int64 curl copt cval =
  withCODE $ ccurl_easy_setopt'Int64 (ccurlptr curl) copt cval

setopt'CLong :: CURL -> CCURLoption'CLong -> CLong -> IO ()
setopt'CLong curl copt cval =
  withCODE $ ccurl_easy_setopt'CLong (ccurlptr curl) copt cval


-------------------------------------------------------------------------------
so'FWRITE :: CURL -> Maybe CURL_write_callback -> IO ()
so'FWRITE curl mcb = makeCallback mcb (cb_write curl)
  (ccurl_easy_setopt'FWRITE (ccurlptr curl))
  (\cb -> wrap_ccurl_write_callback (write_callback cb))

write_callback :: CURL_write_callback -> CCURL_write_callback
write_callback fwrite ptr size nmemb _ = do
  stat <- packCStringLen (ptr, fromIntegral (size * nmemb)) >>= fwrite
  return $ case stat of
    CURL_WRITEFUNC_OK    -> (size * nmemb)
    CURL_WRITEFUNC_FAIL  -> 0
    CURL_WRITEFUNC_PAUSE -> cCURL_WRITEFUNC_PAUSE


-------------------------------------------------------------------------------
so'FREAD :: CURL -> Maybe CURL_read_callback -> IO ()
so'FREAD curl mcb = makeCallback mcb (cb_read curl)
  (ccurl_easy_setopt'FREAD (ccurlptr curl))
  (\cb -> wrap_ccurl_read_callback (read_callback cb))

read_callback :: CURL_read_callback -> CCURL_read_callback
read_callback fread buff size nmemb _ = do
  let buffLen = fromIntegral (size * nmemb)
  stat <- fread buffLen
  case stat of
    CURL_READFUNC_PAUSE -> return cCURL_READFUNC_PAUSE
    CURL_READFUNC_ABORT -> return cCURL_READFUNC_ABORT
    CURL_READFUNC_OK bs -> unsafeUseAsCStringLen (BS.take buffLen bs)
      (\(cs, cl) -> copyBytes buff cs cl >> return (fromIntegral cl))


-------------------------------------------------------------------------------
fromUTCTime :: UTCTime -> CLong
fromUTCTime = truncate . utcTimeToPOSIXSeconds


-------------------------------------------------------------------------------
#{let option opt = #opt " -> c" #opt}

class CURLenum a where
  fromCURLenum :: a -> CLong

instance CURLenum a => CURLenum [a] where
  fromCURLenum xs = foldl' (.|.) 0 $ map fromCURLenum xs

instance CURLenum CURLproto where
  fromCURLenum x = case x of
    #{option CURLPROTO_ALL   }
    #{option CURLPROTO_HTTP  }
    #{option CURLPROTO_HTTPS }
    #{option CURLPROTO_FTP   }
    #{option CURLPROTO_FTPS  }
    #{option CURLPROTO_SCP   }
    #{option CURLPROTO_SFTP  }
    #{option CURLPROTO_TELNET}
    #{option CURLPROTO_LDAP  }
    #{option CURLPROTO_LDAPS }
    #{option CURLPROTO_DICT  }
    #{option CURLPROTO_FILE  }
    #{option CURLPROTO_TFTP  }
    #{option CURLPROTO_IMAP  }
    #{option CURLPROTO_IMAPS }
    #{option CURLPROTO_POP3  }
    #{option CURLPROTO_POP3S }
    #{option CURLPROTO_SMTP  }
    #{option CURLPROTO_SMTPS }
    #{option CURLPROTO_RTSP  }
    #{option CURLPROTO_RTMP  } |7210:----|
    #{option CURLPROTO_RTMPT } |7210:----|
    #{option CURLPROTO_RTMPE } |7210:----|
    #{option CURLPROTO_RTMPTE} |7210:----|
    #{option CURLPROTO_RTMPS } |7210:----|
    #{option CURLPROTO_RTMPTS} |7210:----|
    #{option CURLPROTO_GOPHER} |7212:----|

instance CURLenum CURLproxy where
  fromCURLenum x = case x of
    #{option CURLPROXY_HTTP           }
    #{option CURLPROXY_HTTP_1_0       }
    #{option CURLPROXY_SOCKS4         }
    #{option CURLPROXY_SOCKS5         }
    #{option CURLPROXY_SOCKS4A        }
    #{option CURLPROXY_SOCKS5_HOSTNAME}

instance CURLenum CURLnetrc where
  fromCURLenum x = case x of
    #{option CURL_NETRC_IGNORED }
    #{option CURL_NETRC_OPTIONAL}
    #{option CURL_NETRC_REQUIRED}

instance CURLenum CURLauth where
  fromCURLenum x = case x of
    #{option CURLAUTH_BASIC       }
    #{option CURLAUTH_DIGEST      }
    #{option CURLAUTH_DIGEST_IE   }
    #{option CURLAUTH_GSSNEGOTIATE}
    #{option CURLAUTH_NTLM        }
    #{option CURLAUTH_NTLM_WB     } |7220:----|
    #{option CURLAUTH_ONLY        } |7213:----|
    #{option CURLAUTH_ANY         }
    #{option CURLAUTH_ANYSAFE     }

instance CURLenum CURLtlsauth where                              |7214:----|
  fromCURLenum x = case x of                                     |7214:----|
    #{option CURL_TLSAUTH_SRP}                                   |7214:----|

instance CURLenum CURLredir where
  fromCURLenum x = case x of
    #{option CURL_REDIR_GET_ALL }
    #{option CURL_REDIR_POST_301}
    #{option CURL_REDIR_POST_302}
    #{option CURL_REDIR_POST_ALL}

instance CURLenum CURLhttpver where
  fromCURLenum x = case x of
    #{option CURL_HTTP_VERSION_NONE}
    #{option CURL_HTTP_VERSION_1_0 }
    #{option CURL_HTTP_VERSION_1_1 }

instance CURLenum CURLftpcreate where
  fromCURLenum x = case x of
    #{option CURLFTP_CREATE_DIR_NONE }
    #{option CURLFTP_CREATE_DIR      }
    #{option CURLFTP_CREATE_DIR_RETRY}

instance CURLenum CURLftpauth where
  fromCURLenum x = case x of
    #{option CURLFTPAUTH_DEFAULT}
    #{option CURLFTPAUTH_SSL    }
    #{option CURLFTPAUTH_TLS    }

instance CURLenum CURLftpssl where
  fromCURLenum x = case x of
    #{option CURLFTPSSL_CCC_NONE   }
    #{option CURLFTPSSL_CCC_PASSIVE}
    #{option CURLFTPSSL_CCC_ACTIVE }

instance CURLenum CURLftpmethod where
  fromCURLenum x = case x of
    #{option CURLFTPMETHOD_DEFAULT  }
    #{option CURLFTPMETHOD_MULTICWD }
    #{option CURLFTPMETHOD_NOCWD    }
    #{option CURLFTPMETHOD_SINGLECWD}

instance CURLenum CURLrtspreq where
  fromCURLenum x = case x of
    #{option CURL_RTSPREQ_OPTIONS      }
    #{option CURL_RTSPREQ_DESCRIBE     }
    #{option CURL_RTSPREQ_ANNOUNCE     }
    #{option CURL_RTSPREQ_SETUP        }
    #{option CURL_RTSPREQ_PLAY         }
    #{option CURL_RTSPREQ_PAUSE        }
    #{option CURL_RTSPREQ_TEARDOWN     }
    #{option CURL_RTSPREQ_GET_PARAMETER}
    #{option CURL_RTSPREQ_SET_PARAMETER}
    #{option CURL_RTSPREQ_RECORD       }
    #{option CURL_RTSPREQ_RECEIVE      }

instance CURLenum CURLtimecond where
  fromCURLenum x = case x of
    #{option CURL_TIMECOND_NONE        }
    #{option CURL_TIMECOND_IFMODSINCE  }
    #{option CURL_TIMECOND_IFUNMODSINCE}
    #{option CURL_TIMECOND_LASTMOD     }

instance CURLenum CURLclosepol where
  fromCURLenum x = case x of
    #{option CURLCLOSEPOLICY_NONE               }
    #{option CURLCLOSEPOLICY_OLDEST             }
    #{option CURLCLOSEPOLICY_LEAST_RECENTLY_USED}
    #{option CURLCLOSEPOLICY_LEAST_TRAFFIC      }
    #{option CURLCLOSEPOLICY_SLOWEST            }
    #{option CURLCLOSEPOLICY_CALLBACK           }

instance CURLenum CURLipresolve where
  fromCURLenum x = case x of
    #{option CURL_IPRESOLVE_WHATEVER}
    #{option CURL_IPRESOLVE_V4      }
    #{option CURL_IPRESOLVE_V6      }

instance CURLenum CURLusessl where
  fromCURLenum x = case x of
    #{option CURLUSESSL_NONE   }
    #{option CURLUSESSL_TRY    }
    #{option CURLUSESSL_CONTROL}
    #{option CURLUSESSL_ALL    }

instance CURLenum CURLsslver where
  fromCURLenum x = case x of
    #{option CURL_SSLVERSION_DEFAULT}
    #{option CURL_SSLVERSION_TLSv1  }
    #{option CURL_SSLVERSION_SSLv2  }
    #{option CURL_SSLVERSION_SSLv3  }

instance CURLenum CURLsslopt where                               |7250:----|
  fromCURLenum x = case x of                                     |7250:----|
    #{option CURLSSLOPT_ALLOW_BEAST}                             |7250:----|

instance CURLenum CURLgssapi where                               |7220:----|
  fromCURLenum x = case x of                                     |7220:----|
    #{option CURLGSSAPI_DELEGATION_NONE       }                  |7220:----|
    #{option CURLGSSAPI_DELEGATION_POLICY_FLAG}                  |7220:----|
    #{option CURLGSSAPI_DELEGATION_FLAG       }                  |7220:----|

instance CURLenum CURLsshauth where
  fromCURLenum x = case x of
    #{option CURLSSH_AUTH_ANY      }
    #{option CURLSSH_AUTH_NONE     }
    #{option CURLSSH_AUTH_PUBLICKEY}
    #{option CURLSSH_AUTH_PASSWORD }
    #{option CURLSSH_AUTH_HOST     }
    #{option CURLSSH_AUTH_KEYBOARD }
    #{option CURLSSH_AUTH_DEFAULT  }

