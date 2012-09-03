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
module Network.Protocol.Uri.Path where

import Data.List
import Network.Protocol.Mime
import Data.Label

{- | Label to access the extension of a filename. -}

extension :: FilePath :-> Maybe String
extension = lens getExt setExt
  where
    splt     p = (\(a,b) -> (reverse a, reverse b)) $ break (=='.') $ reverse p
    isExt e  p = '/' `elem` e || not ('.' `elem` p)
    getExt   p = let (u, v) = splt p in
                 if isExt u v then Nothing else Just u
    setExt e p = let (u, v) = splt p in
                 (if isExt u v then p else init v) ++ maybe "" ('.':) e

{- |
Try to guess the correct mime type for the input file based on the file
extension.
-}

mimetype :: FilePath -> Maybe String
mimetype p = get extension p >>= mime

{- |
Normalize a path by removing or merging all dot or dot-dot segments and double
slashes. 
-}

-- Todo: is this windows-safe?  is it really secure?

normalize :: FilePath -> FilePath
normalize p  = norm_rev (reverse p)
  where
    norm_rev ('/':t) = start_dir 0 "/" t
    norm_rev (    t) = start_dir 0 ""  t

    start_dir n q (".."           ) = rest_dir   n    q  ""
    start_dir n q ('/':t          ) = start_dir  n    q  t
    start_dir n q ('.':'/':t      ) = start_dir  n    q  t
    start_dir n q ('.':'.':'/': t ) = start_dir (n+1) q  t
    start_dir n q (t              ) = rest_dir   n    q  t

    rest_dir  n q  ""
        | n > 0      = foldr (++) q (replicate n "../")
        | null q     = "/"
        | otherwise  = q
    rest_dir  0 q ('/':t ) = start_dir  0   ('/':q)  t
    rest_dir  n q ('/':t ) = start_dir (n-1)     q   t
    rest_dir  0 q (h:t   ) = rest_dir   0   (  h:q)  t
    rest_dir  n q (_:t   ) = rest_dir   n        q   t

{- | Jail a filepath within a jail directory. -}

jail
  :: FilePath         -- ^ Jail directory.
  -> FilePath         -- ^ Filename to jail.
  -> Maybe FilePath
jail jailDir p =
  let nj = normalize jailDir
      np = normalize p in
  if nj `isPrefixOf` np -- && not (".." `isPrefixOf` np)
    then Just np
    else Nothing

{- | Concatenate and normalize two filepaths. -}

(/+) :: FilePath -> FilePath -> FilePath
a /+ b = normalize (a ++ "/" ++ b)

