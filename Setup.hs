module Main where

import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import Distribution.PackageDescription    (BuildInfo)
import Distribution.Verbosity             (Verbosity)
import System.FilePath                    ((</>), replaceExtension)
import Text.ParserCombinators.ReadP
import Text.Read.Lex (readDecP)
import Data.IORef    (IORef, newIORef, readIORef, writeIORef)
import Data.List     (find, isPrefixOf)


-------------------------------------------------------------------------------
main = newIORef Nothing >>= \cvr -> defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors = ("hs", ppHsX cvr) : ("hsc", ppHscX cvr)
                        : knownSuffixHandlers }


-------------------------------------------------------------------------------
ppHsX :: IORef (Maybe Int) -> BuildInfo -> LocalBuildInfo -> PreProcessor
ppHsX cvr bi lbi = PreProcessor
  { platformIndependent = False
  , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
      cv <- findCURLVER cvr bi lbi verbosity
      withUTF8FileContents inFile (writeUTF8File outFile . processFile cv)
  }

-------------------------------------------------------------------------------
ppHscX :: IORef (Maybe Int) -> BuildInfo -> LocalBuildInfo -> PreProcessor
ppHscX cvr bi lbi = PreProcessor
  { platformIndependent = False
  , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
      cv <- findCURLVER cvr bi lbi verbosity
      let hscFile = replaceExtension outFile "hsc"
      withUTF8FileContents inFile (writeUTF8File hscFile . processFile cv)
      runSimplePreProcessor (ppHsc2hs bi lbi) hscFile outFile verbosity
  }

-------------------------------------------------------------------------------
findCURLVER :: IORef (Maybe Int)
            -> BuildInfo -> LocalBuildInfo -> Verbosity -> IO Int
findCURLVER cvr bi lbi verbosity = readIORef cvr >>= \mcv -> case mcv of
  Just cv -> return cv
  Nothing -> do
    info verbosity "Looking for LIBCURL_VERSION_NUM in \"curl/curl.h\""
    let cvFile = buildDir lbi </> "CURLVER"
    writeUTF8File cvFile curlverFileContent
    runSimplePreProcessor (ppHsc2hs bi lbi) cvFile cvFile verbosity
    mcv <- fmap parseCV $ readUTF8File cvFile
    case mcv of
      Nothing -> die "Can't find libcurl version info"
      Just cv -> writeIORef cvr mcv >> return cv

curlverFileContent :: String
curlverFileContent = unlines
  [ "#include \"curl/curl.h\""
  , "CURLVER = #{const LIBCURL_VERSION_NUM}" ]

parseCV :: String -> Maybe Int
parseCV cvf = do
  cvl <- find (isPrefixOf "CURLVER") $ lines cvf
  case (readP_to_S readDecP $ last $ words cvl) of
    [(cv,[])] -> Just cv
    _         -> Nothing

-------------------------------------------------------------------------------
processFile :: Int -> String -> String
processFile cv file = unlines $ map (processLine cv) $ lines file

processLine :: Int -> String -> String
processLine cv line =
  case (readP_to_S parseLine $ reverse line) of
    [((vmin, vmax, text),[])]
      -> if (vmin <= cv && cv <= vmax) then text else concat ["-- ", line]
    _ -> line

parseLine :: ReadP (Int, Int, String)
parseLine =
  char '|' >> parseVTAG >>= \vmax ->
  char ':' >> parseVTAG >>= \vmin ->
  char '|' >> parseTEXT >>= \text ->
  return (maybe 0x000000 id vmin, maybe 0xFFFFFF id vmax, text)

parseTEXT :: ReadP String
parseTEXT = skipSpaces >> manyTill get eof >>= return . reverse

parseVTAG :: ReadP (Maybe Int)
parseVTAG = count 4 get >>= \vtag -> if (vtag == "----") then return Nothing
  else maybe pfail (return . Just) $ lookup (reverse vtag)
    [ ("7200", 0x071400)
    , ("7201", 0x071401)
    , ("7210", 0x071500)
    , ("7211", 0x071501)
    , ("7212", 0x071502)
    , ("7213", 0x071503)
    , ("7214", 0x071504)
    , ("7215", 0x071505)
    , ("7216", 0x071506)
    , ("7217", 0x071507)
    , ("7220", 0x071600)
    , ("7230", 0x071700)
    , ("7231", 0x071701)
    , ("7240", 0x071800)
    , ("7250", 0x071900)
    ]

