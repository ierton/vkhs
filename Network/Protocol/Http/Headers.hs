{-
Copyright (c) Sebastiaan Visser 2008

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

-}
{-# LANGUAGE TypeOperators #-}
module Network.Protocol.Http.Headers where {- doc ok -}

import Control.Category
import Control.Monad
import Data.Label as L
import Data.Monoid
import Data.Label.Abstract as A
import Network.Protocol.Http.Data
import Network.Protocol.Uri.Query
import Network.Protocol.Uri
import Network.Protocol.Cookie
import Prelude hiding ((.), id)
import Safe

-- | Access the /Content-Length/ header field.

contentLength :: (Show i, Read i, Integral i) => Http a :-> Maybe i
contentLength = (Bij (join . fmap readMay) (fmap show)) `iso` header "Content-Length"

-- | Access the /Connection/ header field.

connection :: Http a :-> Maybe String
connection = header "Connection"

-- | Access the /Accept/ header field.

accept :: Http a :-> Maybe Parameters
accept = liftBij (keyValues "," ";") `iso` header "Accept"

-- | Access the /Accept-Encoding/ header field.

acceptEncoding :: Http a :-> Maybe [String]
acceptEncoding = liftBij (values ",") `iso` header "Accept-Encoding"

-- | Access the /Accept-Language/ header field.

acceptLanguage :: Http a :-> Maybe [String]
acceptLanguage = liftBij (values ",") `iso` header "Accept-Language"

-- | Access the /Connection/ header field.

cacheControl :: Http a :-> Maybe String
cacheControl = header "Cache-Control"

-- | Access the /Keep-Alive/ header field.

keepAlive :: (Show i, Read i, Integral i) => Http a :-> Maybe i
keepAlive = (Bij (join . fmap readMay) (fmap show)) `iso` header "Keep-Alive"

maybe2list :: Bijection (->) b [c] -> (a :-> (Maybe b)) -> (a :-> [c])
maybe2list b l = L.lens g s where
   g x = maybe [] (fw b) (L.get l x)
   s [] x = L.set l (Nothing) x
   s c x = L.set l (Just $ bw b c) x

-- | Access the /Cookie/ header field.

cookiesShort :: Http Request :-> [CookieShort]
cookiesShort = maybe2list (cookie) (header "Cookie")

-- | Access the /Set-Cookie/ fields.

setCookies :: Http Response :-> Cookies
setCookies = (gather . liftBij setCookie) `iso` (headerMany "Set-Cookie")

-- | Access the /Location/ header field.

location :: Http a :-> Maybe Uri
location = liftBij (Bij toUri showUri) `iso` (header "Location")

-- | Access the /Content-Type/ header field. The content-type will be parsed
-- into a mimetype and optional charset.

contentType :: Http a :-> Maybe (String, String)
contentType = 
        liftBij (Bij parser printer)
  `iso` liftBij (keyValues ";" "=")
  `iso` header "Content-Type"
  where 
    printer (x, y) = (x, "") : ("charset", y) : []
    parser ((m, _):("charset", c):_) = (m, c)

-- | Access the /Date/ header field.

date :: Http a :-> Maybe String
date = header "Date"

-- | Access the /Host/ header field.

hostname :: Http a :-> Maybe String
hostname = header "Host"

-- | Access the /Server/ header field.

server :: Http a :-> Maybe String
server = header "Server"

-- | Access the /User-Agent/ header field.

userAgent :: Http a :-> Maybe String
userAgent = header "User-Agent"

-- | Access the /Upgrade/ header field.

upgrade :: Http a :-> Maybe String
upgrade = header "Upgrade"

-- | Access the /Last-Modified/ header field.

lastModified :: Http a :-> Maybe Value
lastModified = header "Last-Modified"

-- | Access the /Accept-Ranges/ header field.

acceptRanges :: Http a :-> Maybe Value
acceptRanges = header "Accept-Ranges"

-- | Access the /ETag/ header field.

eTag :: Http a :-> Maybe Value
eTag = header "ETag"

