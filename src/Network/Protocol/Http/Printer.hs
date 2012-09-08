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
{-# LANGUAGE FlexibleInstances #-}
module Network.Protocol.Http.Printer where

import Network.Protocol.Http.Data
import Network.Protocol.Http.Status

-- instance Show (Http Request) where
--   showsPrec _ r@(Http Request {} _ hs) =
--     showRequestLine r . shows hs . eol

printRequest :: Http Request -> String
printRequest r@(Http Request {} _ hs) = (showRequestLine r . showHeaders hs . eol) ""

-- instance Show (Http Response) where
--   showsPrec _ r@(Http Response {} _ hs) =
--     showResponseLine r . shows hs . eol

printResponse :: Http Response -> String
printResponse r@(Http Response {} _ hs) = (showResponseLine r . showHeaders hs . eol) ""

-- | Show HTTP request status line.

showRequestLine :: Http Request -> String -> String
showRequestLine (Http (Request m u) v _) =
    shows m . ss " " . ss u . ss " "
  . showVersion v . eol

-- | Show HTTP response status line.

showResponseLine :: Http Response -> String -> String
showResponseLine (Http (Response s) v _) =
    showVersion v . ss " "
  . shows (codeFromStatus s)
  . ss " " . shows s . eol

showHeaders :: Headers -> ShowS
showHeaders =
      foldr (\a b -> a . eol . b) id
    . map (\(k, a) -> ss k . ss ": " . ss a)
    . unHeaders

-- instance Show Headers where
--   showsPrec _ =
--       foldr (\a b -> a . eol . b) id
--     . map (\(k, a) -> ss k . ss ": " . ss a)
--     . unHeaders

showVersion :: Version -> ShowS
showVersion (Version a b) = ss "HTTP/" . shows a . ss "." . shows b

-- instance Show Version where
--   showsPrec _ (Version a b) = ss "HTTP/" . shows a . ss "." . shows b

eol :: ShowS
eol = ss "\r\n"

ss :: String -> ShowS
ss = showString

