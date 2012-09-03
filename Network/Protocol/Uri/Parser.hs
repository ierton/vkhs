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
{-# LANGUAGE
    StandaloneDeriving
  , TypeOperators
  , FlexibleContexts
  , FlexibleInstances
  #-}
module Network.Protocol.Uri.Parser where

import Control.Applicative hiding (empty)
import Control.Category
import Data.Char
import Data.List 
import Data.Maybe
import Data.Label
import Network.Protocol.Uri.Data
import Network.Protocol.Uri.Encode
import Network.Protocol.Uri.Printer
import Network.Protocol.Uri.Query
import Prelude hiding ((.), id, mod)
import Safe
import Text.ParserCombinators.Parsec hiding (many, (<|>))

-- | Access the host part of the URI.

host :: Uri :-> String
host = (Bij showHost ((either (const mkHost) id) . parseHost)) `iso` (_host . authority)

-- | Access the path part of the URI. The query will be properly decoded when
-- reading and encoded when writing.

path :: Uri :-> FilePath
path = (Bij (decode . showPath) (either (const mkPath) id . parsePath . encode)) `iso` _path

-- | Access the path and query parts of the URI as a single string. The string
-- will will be properly decoded when reading and encoded when writing.

pathAndQuery :: Uri :-> String
pathAndQuery = values "?" `osi` Lens ((\p q -> [p, q]) <$> idx 0 `for` path <*> idx 1 `for` query)
  where idx = flip (atDef "")
        osi (Bij a b) = iso (Bij b a)

-- | Parse string into a URI and ignore all failures by returning an empty URI
-- when parsing fails. Can be quite useful in situations that parse errors are
-- unlikely.

toUri :: String -> Uri
toUri = either (const mkUri) id . parseUri

-- | Parse string into a URI.

parseUri :: String -> Either ParseError Uri
parseUri = parse pUriReference ""

-- | Parse string into a URI and only accept absolute URIs.

parseAbsoluteUri :: String -> Either ParseError Uri
parseAbsoluteUri = parse pAbsoluteUri ""

-- | Parse string into an authority.

parseAuthority :: String -> Either ParseError Authority
parseAuthority = parse pAuthority ""

-- | Parse string into a path.

parsePath :: String -> Either ParseError Path
parsePath = parse pPath ""

-- | Parse string into a host.

parseHost :: String -> Either ParseError Host
parseHost = parse pHost ""

-- D.2.  Modifications

pAlpha, pDigit, pAlphanum :: CharParser st Char
pAlpha    = letter
pDigit    = digit
pAlphanum = alphaNum

-- 2.3.  Unreserved Characters

pUnreserved :: GenParser Char st Char
pUnreserved  = pAlphanum <|> oneOf "-._~"

pReserved :: GenParser Char st Char
pReserved  = pGenDelims <|> pSubDelims

pGenDelims :: CharParser st Char
pGenDelims = oneOf ":/?#[]@"

pSubDelims :: CharParser st Char
pSubDelims = oneOf "!$&'()*+,;="

-- 2.1.  Percent-Encoding

pPctEncoded :: GenParser Char st String
pPctEncoded = (:) <$> char '%' <*> pHex

pHex :: GenParser Char st String
pHex = (\a b -> a:b:[])
    <$> hexDigit
    <*> hexDigit

-- 3.  Syntax Components

-- With the hier-part integrated.

pUri :: GenParser Char st Uri
pUri = (\a (b,c) d e -> Uri False a b c d e)
  <$> (pScheme <* string ":")
  <*> (q <|> p)
  <*> option "" (string "?" *> pQuery)
  <*> option "" (string "#" *> pFragment)
  where
    q = (,) <$> (string "//" *> pAuthority) <*> pPathAbempty
    p = ((,) mkAuthority) <$> (pPathAbsolute <|> pPathRootless {-<|> pPathEmpty-})

-- 3.1.  Scheme

pScheme :: GenParser Char st String
pScheme = (:) <$> pAlpha <*> many (pAlphanum <|> oneOf "+_.")

-- 3.2.  Authority

pAuthority :: GenParser Char st Authority
pAuthority = Authority
  <$> option mkUserinfo (try (pUserinfo <* string "@"))
  <*> pHost
  <*> option Nothing (string ":" *> pPort)

-- 3.2.1.  User Information

pUserinfo :: GenParser Char st String
pUserinfo = concat <$> many (
      (pure <$> pUnreserved)
  <|> (         pPctEncoded)
  <|> (pure <$> pSubDelims)
  <|> (pure <$> oneOf ":")
  )

-- 3.2.2.  Host

pHost :: GenParser Char st Host
pHost = diff <$> pRegName -- <|> RegName <$> pRegName
  where
    diff  a = either (const (RegName a)) sep (parse pHostname "" a)
    sep   a = if hst a then Hostname (Domain a) else ipreg a
    ipreg a = if ip a then IP (toIP a) else RegName (intercalate "." a)
    hst     = not . all isDigit . headDef "" . dropWhile null . reverse
    ip    a = length a == 4 && length (mapMaybe (either (const Nothing) Just . parse pDecOctet "") a) == 4
    toIP [a, b, c, d] = IPv4 (read a) (read b) (read c) (read d)
    toIP _            = IPv4 0 0 0 0

{-
pfff, ipv6 is sooo not gonna make it..

pIPLiteral = "[" ( IPv6address <|> IPvFuture  ) "]"

pIPvFuture = "v" 1*HEXDIG "." 1*( unreserved <|> subDelims <|> ":" )

pIPv6address =
                                 6( h16 ":" ) ls32
  <|>                       "::" 5( h16 ":" ) ls32
  <|> [               h16 ] "::" 4( h16 ":" ) ls32
  <|> [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  <|> [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  <|> [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
  <|> [ *4( h16 ":" ) h16 ] "::"              ls32
  <|> [ *5( h16 ":" ) h16 ] "::"              h16
  <|> [ *6( h16 ":" ) h16 ] "::"

pH16           = 1*4HEXDIG
pLs32          = ( h16 ":" h16 ) <|> IPv4address
-}

pIPv4address :: GenParser Char st [Int]
pIPv4address = (:) <$> pDecOctet <*> (count 3 $ char '.' *> pDecOctet)

pDecOctet :: GenParser Char st Int
pDecOctet = read <$> choice [
    try ((\a b c -> [a,b,c]) <$> char '2' <*> char '5'      <*> oneOf "012345")
  , try ((\a b c -> [a,b,c]) <$> char '2' <*> oneOf "01234" <*> digit)
  , try ((\a b c -> [a,b,c]) <$> char '1' <*> digit         <*> digit)
  , try ((\a b   -> [a,b])   <$>              digit         <*> digit)
  ,     (pure                <$>                                digit)
  ]

pRegName :: GenParser Char st String
pRegName = concat <$> many1 (
      (pure <$> pUnreserved)
  <|>           pPctEncoded
  <|> (pure <$> pSubDelims))

-- Not actually part of the rfc3986, but comptability with the rfc2396.
-- This information can be useful, so why throw away.

pHostname :: GenParser Char st [String]
pHostname = sepBy (option "" pDomainlabel) (string ".")

pDomainlabel :: GenParser Char st String
pDomainlabel = intercalate "-" <$> sepBy1 (some pAlphanum) (string "-")

-- 3.2.3.  Port

pPort :: GenParser Char st (Maybe Port)
pPort = readMay <$> some pDigit

-- 3.4.  Query

pQuery :: GenParser Char st String
pQuery = concat <$> many (pPchar <|> pure <$> oneOf "/?")

-- 3.5.  Fragment

pFragment :: GenParser Char st String
pFragment = concat <$> many (pPchar <|> pure <$> oneOf "/?" )

-- 3.3.  Path

pPath, pPathAbempty, pPathAbsolute, pPathNoscheme, pPathRootless, pPathEmpty :: GenParser Char st Path

pPath =
      try pPathAbsolute -- begins with "/" but not "//"
  <|> try pPathNoscheme -- begins with a nonColon segment
  <|> try pPathRootless -- begins with a segment
  <|> pPathEmpty        -- zero characters

pPathAbempty  = Path . ("":) <$> _pSlashSegments
pPathAbsolute = (char '/' *>) $ Path . ("":) <$> (option [] $ (:) <$> pSegmentNz <*> _pSlashSegments)
pPathNoscheme = Path <$> ((:) <$> pSegmentNzNc <*> _pSlashSegments)
pPathRootless = Path <$> ((:) <$> pSegmentNz    <*> _pSlashSegments)
pPathEmpty    = Path [] <$ string ""

pSegment, pSegmentNz, pSegmentNzNc :: GenParser Char st String
pSegment     = concat <$> many pPchar
pSegmentNz   = concat <$> some pPchar
pSegmentNzNc = concat <$> some (
      (pure <$> pUnreserved)
  <|>           pPctEncoded
  <|> (pure <$> pSubDelims)
  <|> (pure <$> oneOf "@" ))

_pSlashSegments :: GenParser Char st [PathSegment]
_pSlashSegments = (many $ (:) <$> char '/' *> pSegment)


pPchar :: GenParser Char st String
pPchar = choice
  [ pure <$> pUnreserved
  , pPctEncoded
  , pure <$> pSubDelims
  , pure <$> oneOf ":@"
  ]

-- 4.1.  URI Reference

pUriReference :: GenParser Char st Uri
pUriReference = try pAbsoluteUri <|> pRelativeRef

-- 4.2.  Relative Reference

-- With the relative-part integrated.

pRelativeRef :: GenParser Char st Uri
pRelativeRef = ($)
  <$> (try pRelativePart
  <|> ((Uri True mkScheme mkAuthority)
  <$> (pPathAbsolute <|> pPathRootless <|> pPathEmpty)))
  <*> option "" (string "?" *> pQuery)
  <*> option "" (string "#" *> pFragment)

pRelativePart :: GenParser Char st (Query -> Fragment -> Uri)
pRelativePart = Uri True mkScheme <$> (string "//" *> pAuthority) <*> pPathAbempty

-- 4.3.  Absolute URI

pAbsoluteUri :: GenParser Char st Uri
pAbsoluteUri = pUri
