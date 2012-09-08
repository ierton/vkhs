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
module Network.Protocol.Uri.Query where

import Prelude hiding ((.), id)
import Control.Category
import Data.List
import Data.List.Split 
import Data.Label
import Network.Protocol.Uri.Data
import Network.Protocol.Uri.Encode

type Parameters = [(String, String)]

-- | Fetch the query parameters form a URI.

queryParams :: Uri :-> Parameters
queryParams = params `iso` _query

-- | Generic lens to parse/print a string as query parameters.

params :: Bijection (->) String Parameters
params = keyValues "&" "=" . (Bij from to) . encoded
  where from = intercalate " " . splitOn "+"
        to   = intercalate "+" . splitOn " "

-- | Generic label for accessing key value pairs encoded in a string.

keyValues :: String -> String -> Bijection (->) String Parameters
keyValues sep eqs = Bij parser printer
  where parser =
            filter (\(a, b) -> not (null a))
          . map (f . splitOn eqs)
          . concat
          . map (splitOn sep)
          . lines
          where f []     = ("", "")
                f [x]    = (trim x, "")
                f (x:xs) = (trim x, trim $ intercalate eqs xs)
        printer = intercalate sep . map (\(a, b) -> a ++ eqs ++ b)

-- | Generic label for accessing lists of values encoded in a string.

values :: String -> Bijection (->) String [String]
values sep = Bij parser printer
  where parser = filter (not . null) . concat . map (map trim . splitOn sep) . lines
        printer = intercalate sep

-- Helper to trim all heading and trailing whitespace.

trim :: String -> String
trim = rev (dropWhile (`elem` " \t\n\r"))
  where rev f = reverse . f . reverse . f

