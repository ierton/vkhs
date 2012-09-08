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
module Network.Protocol.Uri.Encode where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.Label
import Network.Protocol.Uri.Chars
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

-- | URI encode a string.

encode :: String -> String
encode = concatMap encodeChr
  where
    encodeChr c
      | unreserved c || genDelims c || subDelims c = [c]
      | otherwise = '%' :
          intToDigit (shiftR (ord c) 4) :
          intToDigit ((ord c) .&. 0x0F) : []

-- | URI decode a string.

decode :: String -> String
decode = U.toString . B.pack . map (fromIntegral . ord) . dec
  where
    dec [] = []
    dec ('%':d:e:ds) | isHexDigit d && isHexDigit e = (chr $ digs d * 16 + digs e) : dec ds 
      where digs a = fromJust $ lookup (toLower a) $ zip "0123456789abcdef" [0..]
    dec (d:ds) = d : dec ds

-- | Decoding and encoding as a label.

encoded :: Bijection (->) String String
encoded = Bij decode encode

