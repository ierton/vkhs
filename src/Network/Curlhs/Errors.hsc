-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Curlhs.Errors
-- Copyright   :  Copyright © 2012 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Network.Curlhs.Errors
  ( curl_easy_strerror
  , withCODE
  ) where

import Data.ByteString   (ByteString, packCString)

import Control.Exception (throwIO)
import Control.Monad     (when)

import Network.Curlhs.Types
import Network.Curlhs.Base


-------------------------------------------------------------------------------
withCODE :: IO CCURLcode -> IO ()
withCODE action =
  action >>= \code -> when (code /= cCURLE_OK) (throwIO (fromCCURLcode code))


-------------------------------------------------------------------------------
-- | Returns a string describing error code
--   (<http://curl.haxx.se/libcurl/c/curl_easy_strerror.html>).
-------------------------------------------------------------------------------
curl_easy_strerror :: CURLcode -> IO ByteString
curl_easy_strerror code =
  ccurl_easy_strerror (fromCURLcode code) >>= packCString


-------------------------------------------------------------------------------
#{let curlcode code = #code " -> c" #code}

fromCURLcode :: CURLcode -> CCURLcode
fromCURLcode x = case x of
  #{curlcode CURLE_OK                      }
  #{curlcode CURLE_UNSUPPORTED_PROTOCOL    }
  #{curlcode CURLE_FAILED_INIT             }
  #{curlcode CURLE_URL_MALFORMAT           }
  #{curlcode CURLE_NOT_BUILT_IN            } |7215:----|
  #{curlcode CURLE_COULDNT_RESOLVE_PROXY   }
  #{curlcode CURLE_COULDNT_RESOLVE_HOST    }
  #{curlcode CURLE_COULDNT_CONNECT         }
  #{curlcode CURLE_FTP_WEIRD_SERVER_REPLY  }
  #{curlcode CURLE_REMOTE_ACCESS_DENIED    }
  #{curlcode CURLE_FTP_ACCEPT_FAILED       } |7240:----|
  #{curlcode CURLE_FTP_WEIRD_PASS_REPLY    }
  #{curlcode CURLE_FTP_ACCEPT_TIMEOUT      } |7240:----|
  #{curlcode CURLE_FTP_WEIRD_PASV_REPLY    }
  #{curlcode CURLE_FTP_WEIRD_227_FORMAT    }
  #{curlcode CURLE_FTP_CANT_GET_HOST       }
  #{curlcode CURLE_FTP_COULDNT_SET_TYPE    }
  #{curlcode CURLE_PARTIAL_FILE            }
  #{curlcode CURLE_FTP_COULDNT_RETR_FILE   }
  #{curlcode CURLE_QUOTE_ERROR             }
  #{curlcode CURLE_HTTP_RETURNED_ERROR     }
  #{curlcode CURLE_WRITE_ERROR             }
  #{curlcode CURLE_UPLOAD_FAILED           }
  #{curlcode CURLE_READ_ERROR              }
  #{curlcode CURLE_OUT_OF_MEMORY           }
  #{curlcode CURLE_OPERATION_TIMEDOUT      }
  #{curlcode CURLE_FTP_PORT_FAILED         }
  #{curlcode CURLE_FTP_COULDNT_USE_REST    }
  #{curlcode CURLE_RANGE_ERROR             }
  #{curlcode CURLE_HTTP_POST_ERROR         }
  #{curlcode CURLE_SSL_CONNECT_ERROR       }
  #{curlcode CURLE_BAD_DOWNLOAD_RESUME     }
  #{curlcode CURLE_FILE_COULDNT_READ_FILE  }
  #{curlcode CURLE_LDAP_CANNOT_BIND        }
  #{curlcode CURLE_LDAP_SEARCH_FAILED      }
  #{curlcode CURLE_FUNCTION_NOT_FOUND      }
  #{curlcode CURLE_ABORTED_BY_CALLBACK     }
  #{curlcode CURLE_BAD_FUNCTION_ARGUMENT   }
  #{curlcode CURLE_INTERFACE_FAILED        }
  #{curlcode CURLE_TOO_MANY_REDIRECTS      }
  #{curlcode CURLE_UNKNOWN_TELNET_OPTION   } |----:7214|
  #{curlcode CURLE_UNKNOWN_OPTION          } |7215:----|
  #{curlcode CURLE_TELNET_OPTION_SYNTAX    }
  #{curlcode CURLE_PEER_FAILED_VERIFICATION}
  #{curlcode CURLE_GOT_NOTHING             }
  #{curlcode CURLE_SSL_ENGINE_NOTFOUND     }
  #{curlcode CURLE_SSL_ENGINE_SETFAILED    }
  #{curlcode CURLE_SEND_ERROR              }
  #{curlcode CURLE_RECV_ERROR              }
  #{curlcode CURLE_SSL_CERTPROBLEM         }
  #{curlcode CURLE_SSL_CIPHER              }
  #{curlcode CURLE_SSL_CACERT              }
  #{curlcode CURLE_BAD_CONTENT_ENCODING    }
  #{curlcode CURLE_LDAP_INVALID_URL        }
  #{curlcode CURLE_FILESIZE_EXCEEDED       }
  #{curlcode CURLE_USE_SSL_FAILED          }
  #{curlcode CURLE_SEND_FAIL_REWIND        }
  #{curlcode CURLE_SSL_ENGINE_INITFAILED   }
  #{curlcode CURLE_LOGIN_DENIED            }
  #{curlcode CURLE_TFTP_NOTFOUND           }
  #{curlcode CURLE_TFTP_PERM               }
  #{curlcode CURLE_REMOTE_DISK_FULL        }
  #{curlcode CURLE_TFTP_ILLEGAL            }
  #{curlcode CURLE_TFTP_UNKNOWNID          }
  #{curlcode CURLE_REMOTE_FILE_EXISTS      }
  #{curlcode CURLE_TFTP_NOSUCHUSER         }
  #{curlcode CURLE_CONV_FAILED             }
  #{curlcode CURLE_CONV_REQD               }
  #{curlcode CURLE_SSL_CACERT_BADFILE      }
  #{curlcode CURLE_REMOTE_FILE_NOT_FOUND   }
  #{curlcode CURLE_SSH                     }
  #{curlcode CURLE_SSL_SHUTDOWN_FAILED     }
  #{curlcode CURLE_AGAIN                   }
  #{curlcode CURLE_SSL_CRL_BADFILE         }
  #{curlcode CURLE_SSL_ISSUER_ERROR        }
  #{curlcode CURLE_FTP_PRET_FAILED         }
  #{curlcode CURLE_RTSP_CSEQ_ERROR         }
  #{curlcode CURLE_RTSP_SESSION_ERROR      }
  #{curlcode CURLE_FTP_BAD_FILE_LIST       } |7210:----|
  #{curlcode CURLE_CHUNK_FAILED            } |7210:----|


-------------------------------------------------------------------------------
#{let ccurlcode code = "| x == c" #code " = " #code}

fromCCURLcode :: CCURLcode -> CURLcode
fromCCURLcode x
  #{ccurlcode CURLE_OK                      }
  #{ccurlcode CURLE_UNSUPPORTED_PROTOCOL    }
  #{ccurlcode CURLE_FAILED_INIT             }
  #{ccurlcode CURLE_URL_MALFORMAT           }
  #{ccurlcode CURLE_NOT_BUILT_IN            } |7215:----|
  #{ccurlcode CURLE_COULDNT_RESOLVE_PROXY   }
  #{ccurlcode CURLE_COULDNT_RESOLVE_HOST    }
  #{ccurlcode CURLE_COULDNT_CONNECT         }
  #{ccurlcode CURLE_FTP_WEIRD_SERVER_REPLY  }
  #{ccurlcode CURLE_REMOTE_ACCESS_DENIED    }
  #{ccurlcode CURLE_FTP_ACCEPT_FAILED       } |7240:----|
  #{ccurlcode CURLE_FTP_WEIRD_PASS_REPLY    }
  #{ccurlcode CURLE_FTP_ACCEPT_TIMEOUT      } |7240:----|
  #{ccurlcode CURLE_FTP_WEIRD_PASV_REPLY    }
  #{ccurlcode CURLE_FTP_WEIRD_227_FORMAT    }
  #{ccurlcode CURLE_FTP_CANT_GET_HOST       }
  #{ccurlcode CURLE_FTP_COULDNT_SET_TYPE    }
  #{ccurlcode CURLE_PARTIAL_FILE            }
  #{ccurlcode CURLE_FTP_COULDNT_RETR_FILE   }
  #{ccurlcode CURLE_QUOTE_ERROR             }
  #{ccurlcode CURLE_HTTP_RETURNED_ERROR     }
  #{ccurlcode CURLE_WRITE_ERROR             }
  #{ccurlcode CURLE_UPLOAD_FAILED           }
  #{ccurlcode CURLE_READ_ERROR              }
  #{ccurlcode CURLE_OUT_OF_MEMORY           }
  #{ccurlcode CURLE_OPERATION_TIMEDOUT      }
  #{ccurlcode CURLE_FTP_PORT_FAILED         }
  #{ccurlcode CURLE_FTP_COULDNT_USE_REST    }
  #{ccurlcode CURLE_RANGE_ERROR             }
  #{ccurlcode CURLE_HTTP_POST_ERROR         }
  #{ccurlcode CURLE_SSL_CONNECT_ERROR       }
  #{ccurlcode CURLE_BAD_DOWNLOAD_RESUME     }
  #{ccurlcode CURLE_FILE_COULDNT_READ_FILE  }
  #{ccurlcode CURLE_LDAP_CANNOT_BIND        }
  #{ccurlcode CURLE_LDAP_SEARCH_FAILED      }
  #{ccurlcode CURLE_FUNCTION_NOT_FOUND      }
  #{ccurlcode CURLE_ABORTED_BY_CALLBACK     }
  #{ccurlcode CURLE_BAD_FUNCTION_ARGUMENT   }
  #{ccurlcode CURLE_INTERFACE_FAILED        }
  #{ccurlcode CURLE_TOO_MANY_REDIRECTS      }
  #{ccurlcode CURLE_UNKNOWN_TELNET_OPTION   } |----:7214|
  #{ccurlcode CURLE_UNKNOWN_OPTION          } |7215:----|
  #{ccurlcode CURLE_TELNET_OPTION_SYNTAX    }
  #{ccurlcode CURLE_PEER_FAILED_VERIFICATION}
  #{ccurlcode CURLE_GOT_NOTHING             }
  #{ccurlcode CURLE_SSL_ENGINE_NOTFOUND     }
  #{ccurlcode CURLE_SSL_ENGINE_SETFAILED    }
  #{ccurlcode CURLE_SEND_ERROR              }
  #{ccurlcode CURLE_RECV_ERROR              }
  #{ccurlcode CURLE_SSL_CERTPROBLEM         }
  #{ccurlcode CURLE_SSL_CIPHER              }
  #{ccurlcode CURLE_SSL_CACERT              }
  #{ccurlcode CURLE_BAD_CONTENT_ENCODING    }
  #{ccurlcode CURLE_LDAP_INVALID_URL        }
  #{ccurlcode CURLE_FILESIZE_EXCEEDED       }
  #{ccurlcode CURLE_USE_SSL_FAILED          }
  #{ccurlcode CURLE_SEND_FAIL_REWIND        }
  #{ccurlcode CURLE_SSL_ENGINE_INITFAILED   }
  #{ccurlcode CURLE_LOGIN_DENIED            }
  #{ccurlcode CURLE_TFTP_NOTFOUND           }
  #{ccurlcode CURLE_TFTP_PERM               }
  #{ccurlcode CURLE_REMOTE_DISK_FULL        }
  #{ccurlcode CURLE_TFTP_ILLEGAL            }
  #{ccurlcode CURLE_TFTP_UNKNOWNID          }
  #{ccurlcode CURLE_REMOTE_FILE_EXISTS      }
  #{ccurlcode CURLE_TFTP_NOSUCHUSER         }
  #{ccurlcode CURLE_CONV_FAILED             }
  #{ccurlcode CURLE_CONV_REQD               }
  #{ccurlcode CURLE_SSL_CACERT_BADFILE      }
  #{ccurlcode CURLE_REMOTE_FILE_NOT_FOUND   }
  #{ccurlcode CURLE_SSH                     }
  #{ccurlcode CURLE_SSL_SHUTDOWN_FAILED     }
  #{ccurlcode CURLE_AGAIN                   }
  #{ccurlcode CURLE_SSL_CRL_BADFILE         }
  #{ccurlcode CURLE_SSL_ISSUER_ERROR        }
  #{ccurlcode CURLE_FTP_PRET_FAILED         }
  #{ccurlcode CURLE_RTSP_CSEQ_ERROR         }
  #{ccurlcode CURLE_RTSP_SESSION_ERROR      }
  #{ccurlcode CURLE_FTP_BAD_FILE_LIST       } |7210:----|
  #{ccurlcode CURLE_CHUNK_FAILED            } |7210:----|
  | otherwise = error "unknown CURLcode"


