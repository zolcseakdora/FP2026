module Roman  where

import Data.List 

-- Római számok értékei csökkenő sorrendben az átváltáshoz
romanGlyphs :: [(String, Int)]
romanGlyphs = [("M", 1000), ("CM", 900), ("D", 500), ("CD", 400),
               ("C", 100),   ("XC", 90),  ("L", 50),  ("XL", 40),
               ("X", 10),    ("IX", 9),   ("V", 5),   ("IV", 4),
               ("I", 1)]

-- 1. Római számból arab szám
romanToArab :: String -> Int
romanToArab "" = 0
romanToArab s = worker romanGlyphs s
  where
    worker [] _ = 0
    worker _ "" = 0
    worker ((rom, arab):g) str
        | rom `isPrefixOf` str = arab + worker ((rom, arab):g) (drop (length rom) str)
        | otherwise            = worker g str

-- 2. Arab számból római szám
arabToRoman :: Int -> String
arabToRoman n
    | n <= 0 || n > 3999 = "HIBA (Csak 1-3999 között értelmezett)"
    | otherwise          = worker romanGlyphs n
  where
    worker [] _ = ""
    worker ((rom, arab):g) val
        | val >= arab = rom ++ worker ((rom, arab):g) (val - arab)
        | otherwise   = worker g val