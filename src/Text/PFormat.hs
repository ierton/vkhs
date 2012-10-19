module Text.PFormat where

import Data.List
import Data.Maybe

-- | Example: pformat '%' [('%',"%"), ('w',"world")] "hello %% %x %w"
pformat :: Char -> [(Char,String)] -> String -> String
pformat x d s = reverse $ scan [] s where
  scan h (c:m:cs)
    | c == x = scan ((reverse $ fromMaybe (c:m:[]) (lookup m d))++h) cs
    | otherwise = scan (c:h) (m:cs)
  scan h (c:[]) = c:h
  scan h [] = h

pformat' :: Char -> [(Char,a->String)] -> String -> a -> String
pformat' x d s a = reverse $ scan [] s where
  scan h (c:m:cs)
    | c == x = scan ((reverse $ fromMaybe (const $ c:m:[]) (lookup m d) $ a)++h) cs
    | otherwise = scan (c:h) (m:cs)
  scan h (c:[]) = c:h
  scan h [] = h

