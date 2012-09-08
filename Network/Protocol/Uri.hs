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

{-# LANGUAGE TemplateHaskell #-}
module Network.Protocol.Uri (

    {- | See rfc2396 for more info. -}

  -- * URI datatype.

    Scheme
  , RegName
  , Port
  , Query
  , Fragment
  , Hash
  , UserInfo
  , PathSegment
  , Parameters

  , Domain (..)
  , IPv4 (..)
  , Path (..)
  , Host (..)
  , Authority (..)
  , Uri (..)

  -- * Accessing parts of URIs.

  , relative
  , scheme
  , userinfo
  , authority
  , host
  , domain
  , ipv4
  , regname
  , port
  , path
  , segments
  , query
  , fragment

  -- * More advanced labels and functions.

  , pathAndQuery
  , queryParams
  , params
  , extension

  , remap

  -- * Encoding/decoding URI encoded strings.

  , encode
  , decode
  , encoded

  -- * Creating empty URIs.

  , mkUri
  , mkScheme
  , mkPath
  , mkAuthority
  , mkQuery
  , mkFragment
  , mkUserinfo
  , mkHost
  , mkPort

  -- * Parsing URIs.

  , toUri
  , parseUri
  , parseAbsoluteUri
  , parseAuthority
  , parsePath
  , parseHost

  -- * Printing URIs
  , showUri
  , showPath
  , showAuthority

  -- * Filename related utilities.

  , mimetype
  , normalize
  , jail
  , (/+)

  ) where

import Network.Protocol.Uri.Data
import Network.Protocol.Uri.Encode
import Network.Protocol.Uri.Parser
import Network.Protocol.Uri.Path
import Network.Protocol.Uri.Printer
import Network.Protocol.Uri.Query
import Network.Protocol.Uri.Remap

