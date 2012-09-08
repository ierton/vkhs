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
module Network.Protocol.Http.Status where {- doc ok -}

import Data.Bimap
import Data.Maybe
import Prelude hiding (lookup)

{- | HTTP status codes. -}

data Status =
    Continue                     -- ^ 100
  | SwitchingProtocols           -- ^ 101
  | OK                           -- ^ 200
  | Created                      -- ^ 201
  | Accepted                     -- ^ 202
  | NonAuthoritativeInformation  -- ^ 203
  | NoContent                    -- ^ 204
  | ResetContent                 -- ^ 205
  | PartialContent               -- ^ 206
  | MultipleChoices              -- ^ 300
  | MovedPermanently             -- ^ 301
  | Found                        -- ^ 302
  | SeeOther                     -- ^ 303
  | NotModified                  -- ^ 304
  | UseProxy                     -- ^ 305
  | TemporaryRedirect            -- ^ 307
  | BadRequest                   -- ^ 400
  | Unauthorized                 -- ^ 401
  | PaymentRequired              -- ^ 402
  | Forbidden                    -- ^ 403
  | NotFound                     -- ^ 404
  | MethodNotAllowed             -- ^ 405
  | NotAcceptable                -- ^ 406
  | ProxyAuthenticationRequired  -- ^ 407
  | RequestTimeOut               -- ^ 408
  | Conflict                     -- ^ 409
  | Gone                         -- ^ 410
  | LengthRequired               -- ^ 411
  | PreconditionFailed           -- ^ 412
  | RequestEntityTooLarge        -- ^ 413
  | RequestURITooLarge           -- ^ 414
  | UnsupportedMediaType         -- ^ 415
  | RequestedRangeNotSatisfiable -- ^ 416
  | ExpectationFailed            -- ^ 417
  | InternalServerError          -- ^ 500
  | NotImplemented               -- ^ 501
  | BadGateway                   -- ^ 502
  | ServiceUnavailable           -- ^ 503
  | GatewayTimeOut               -- ^ 504
  | HTTPVersionNotSupported      -- ^ 505
  | CustomStatus Int String
  deriving (Show, Read, Eq, Ord)

-- | rfc2616 sec6.1.1 Status Code and Reason Phrase.

printStatus :: Status -> String
printStatus Continue                     = "Continue"
printStatus SwitchingProtocols           = "Switching Protocols"
printStatus OK                           = "OK"
printStatus Created                      = "Created"
printStatus Accepted                     = "Accepted"
printStatus NonAuthoritativeInformation  = "Non-Authoritative Information"
printStatus NoContent                    = "No Content"
printStatus ResetContent                 = "Reset Content"
printStatus PartialContent               = "Partial Content"
printStatus MultipleChoices              = "Multiple Choices"
printStatus MovedPermanently             = "Moved Permanently"
printStatus Found                        = "Found"
printStatus SeeOther                     = "See Other"
printStatus NotModified                  = "Not Modified"
printStatus UseProxy                     = "Use Proxy"
printStatus TemporaryRedirect            = "Temporary Redirect"
printStatus BadRequest                   = "Bad Request"
printStatus Unauthorized                 = "Unauthorized"
printStatus PaymentRequired              = "Payment Required"
printStatus Forbidden                    = "Forbidden"
printStatus NotFound                     = "Not Found"
printStatus MethodNotAllowed             = "Method Not Allowed"
printStatus NotAcceptable                = "Not Acceptable"
printStatus ProxyAuthenticationRequired  = "Proxy Authentication Required"
printStatus RequestTimeOut               = "Request Time-out"
printStatus Conflict                     = "Conflict"
printStatus Gone                         = "Gone"
printStatus LengthRequired               = "Length Required"
printStatus PreconditionFailed           = "Precondition Failed"
printStatus RequestEntityTooLarge        = "Request Entity Too Large"
printStatus RequestURITooLarge           = "Request-URI Too Large"
printStatus UnsupportedMediaType         = "Unsupported Media Type"
printStatus RequestedRangeNotSatisfiable = "Requested range not satisfiable"
printStatus ExpectationFailed            = "Expectation Failed"
printStatus InternalServerError          = "Internal Server Error"
printStatus NotImplemented               = "Not Implemented"
printStatus BadGateway                   = "Bad Gateway"
printStatus ServiceUnavailable           = "Service Unavailable"
printStatus GatewayTimeOut               = "Gateway Time-out"
printStatus HTTPVersionNotSupported      = "HTTP Version not supported"
printStatus (CustomStatus _ s)           = s

{- |
RFC2616 sec6.1.1 Status Code and Reason Phrase.

Bidirectional mapping from status numbers to codes.
-}

statusCodes :: Bimap Int Status
statusCodes = fromList [
    (100, Continue)
  , (101, SwitchingProtocols)
  , (200, OK)
  , (201, Created)
  , (202, Accepted)
  , (203, NonAuthoritativeInformation)
  , (204, NoContent)
  , (205, ResetContent)
  , (206, PartialContent)
  , (300, MultipleChoices)
  , (301, MovedPermanently)
  , (302, Found)
  , (303, SeeOther)
  , (304, NotModified)
  , (305, UseProxy)
  , (307, TemporaryRedirect)
  , (400, BadRequest)
  , (401, Unauthorized)
  , (402, PaymentRequired)
  , (403, Forbidden)
  , (404, NotFound)
  , (405, MethodNotAllowed)
  , (406, NotAcceptable)
  , (407, ProxyAuthenticationRequired)
  , (408, RequestTimeOut)
  , (409, Conflict)
  , (410, Gone)
  , (411, LengthRequired)
  , (412, PreconditionFailed)
  , (413, RequestEntityTooLarge)
  , (414, RequestURITooLarge)
  , (415, UnsupportedMediaType)
  , (416, RequestedRangeNotSatisfiable)
  , (417, ExpectationFailed)
  , (500, InternalServerError)
  , (501, NotImplemented)
  , (502, BadGateway)
  , (503, ServiceUnavailable)
  , (504, GatewayTimeOut)
  , (505, HTTPVersionNotSupported)
  ]

-- | Every status greater-than or equal to 400 is considered to be a failure.
statusFailure :: Status -> Bool
statusFailure st = codeFromStatus st >= 400

-- | Conversion from status numbers to codes.
statusFromCode :: Int -> Status
statusFromCode num =
    fromMaybe (CustomStatus num "Unknown Status")
  $ lookup num statusCodes

-- | Conversion from status codes to numbers.
codeFromStatus :: Status -> Int
codeFromStatus (CustomStatus i _) = i
codeFromStatus st =
    fromMaybe 0 -- function is total, should not happen.
  $ lookupR st statusCodes

