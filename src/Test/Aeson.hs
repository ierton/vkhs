{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Error

import System.Directory
import System.Exit
import System.FilePath
import System.Environment
import System.IO

import Data.Aeson as J
import Data.ByteString.Lazy.Char8 as BS (readFile)

main = do
  (a:_) <- getArgs
  s <- BS.readFile a
  let (v :: Maybe J.Value) = J.decode s
  putStrLn $ show v
