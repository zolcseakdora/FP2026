module Password where

import Data.Char 
import Data.List 

type Hash = Integer
type Salt = String
type KDF  = Salt -> String -> String

data Password = P Hash Salt deriving (Show, Eq)

-- ==========================================
-- Szöveg leképezése egész számmá (2 pont)
-- ==========================================
hash :: String -> Hash
hash s = sum [ toInteger i * (2 ^ ord c) | (i, c) <- zip [1..] s ]

-- ==========================================
-- Kulcsleképző függvény I. (1 pont)
-- ==========================================
kdf1 :: KDF
kdf1 salt pwd = pwd ++ salt ++ reverse pwd

-- ==========================================
-- Kulcsleképző függvény II. (1 pont)
-- ==========================================
kdf2 :: Int -> KDF
kdf2 len salt pwd
    | null salt && null pwd = ""
    | null salt             = take len (cycle pwd)
    | otherwise             = take len (pwd ++ cycle salt)

-- ==========================================
-- Jelszavak képzése (2 pont)
-- ==========================================
mkPassword :: KDF -> Salt -> String -> Password
mkPassword kdf salt pwd = P (hash (kdf salt pwd)) salt

-- ==========================================
-- Jelszavak ellenőrzése (3 pont)
-- ==========================================
checkPassword :: Password -> [KDF] -> String -> Bool
checkPassword (P h salt) kdfs pwd =
    let generaltHashek = [ hash (kdf salt pwd) | kdf <- kdfs ]
    in h `elem` generaltHashek

-- ==========================================
-- Segédstruktúrák a szöveges formátumhoz
-- ==========================================
type Alphabet  = [Char]
type Separator = Char

alphabet :: Alphabet
alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

-- ==========================================
-- Egész számok kódolása karaktersorozattá (3 pont)
-- ==========================================
numToWord :: Alphabet -> Integer -> String
numToWord alpha n
    | n == 0    = [head alpha]
    | otherwise = worker n
  where
    base = toInteger (length alpha)
    worker 0 = ""
    worker x = let (q, r) = x `divMod` base
               in (alpha !! fromInteger r) : worker q

-- ==========================================
-- Jelszavak szöveges alakra hozása (2 pont)
-- ==========================================
renderPassword :: Alphabet -> Separator -> Password -> String
renderPassword alpha sep (P h salt) = numToWord alpha h ++ [sep] ++ salt

-- ==========================================
-- Karaktersorozatok átalakítása egész számmá (4 pont)
-- ==========================================
wordToNum :: Alphabet -> String -> Maybe Integer
wordToNum alpha s = worker (zip [0..] s) 0
  where
    base = toInteger (length alpha)
    worker [] acc = Just acc
    worker ((pos, c):cs) acc =
        case elemIndex c alpha of
            Nothing -> Nothing
            Just idx -> worker cs (acc + toInteger idx * (base ^ pos))

-- ==========================================
-- Jelszavak beolvasása szövegből (3 pont)
-- ==========================================
parsePassword :: Alphabet -> Separator -> String -> Maybe Password
parsePassword alpha sep s =
    let (wNum, wSalt) = break (== sep) s
    in if null wSalt 
       then Nothing -- Nincs elválasztójel
       else case wordToNum alpha wNum of
                Nothing -> Nothing
                Just h  -> Just (P h (tail wSalt))