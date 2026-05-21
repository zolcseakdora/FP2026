import Data.List
import Data.Char

{- 9. labor
I. Formázzuk egy adott szövegállomány tartalmát a következőképpen: azok után az írásjelek után, amelyek benne vannak 
a $\{.,!?;\}$ halmazban szigorúan egy szóközt tegyünk, hagyjunk.
-}
-- Segédfüggvény: megkeresi az írásjeleket és javítja a szóközt
fixPunctuation :: String -> String
fixPunctuation [] = []  -- Ha üres a szöveg, kész vagyunk
fixPunctuation [x] = [x] -- Az utolsó karakter után nem kell szóköz
fixPunctuation (x:y:xs)
    | x `elem` ".!?; " && y /= ' ' = x : ' ' : fixPunctuation (y:xs) -- Ha írásjel és nem szóköz jön, szúrunk be egyet
    | otherwise = x : fixPunctuation (y:xs) -- Egyébként megyünk tovább

-- A fájlkezelő rész
task1 :: IO ()
task1 = do
    tartalom <- readFile "szoveg.txt"
    writeFile "szoveg_javitott.txt" (fixPunctuation tartalom)

{-II. Az [iban.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/iban.txt) állomány IBAN kódokat tartalmaz. 
Írjunk egy-egy Haskell függvényt, amely

- beolvassa, majd rendezi az állományban levő adatokat ábécé sorrendbe,
- bináris keresést alkalmazva ellenőrzi, hogy egy megadott IBAN kód szerepel-e az adatok között,
- átírja egy okIban.txt állományba azokat az IBAN kódokat, amelyek megfelelő formátumúak. Egy IBAN kód akkor tekinthető megfelelő formátumúnak
  - ha csak számjegyeket és angol ábécébeli nagybetűket tartalmaz,
  - ha az IBAN kód hossza megegyezik az országhoz tartozó hosszal, ahol az országhoz tartozó hosszérték az [ibanLength.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/ibanLength.txt) állományból olvasható ki,
  - ha az átcsoportosítás és a helyettesítés után kapott egész szám 97-el való osztási maradéka egyenlő eggyel, ahol
    - átcsoportosítás: az IBAN kód első négy karakterét kitöröljük a kód elejéről és a kód végéhez fűzzük,
    - helyettesítés:
      - az alfanumerikus karaktereket helyettesítsük a következő kódokkal: $$A \to 10,\ B \to 11,\ \ldots,\ Z \to 35$$
      - az így kapott karakterláncot egész számnak tekintjük

  Például:
  legyen az IBAN kód: $$\texttt{GB82WEST12345698765432}$$
  - hossz: $$22$$
  - átcsoportosítás:
    $$\texttt{WEST12345698765432}\ \texttt{GB82}$$
  - helyettesítés:
    $$32142829\quad 12345698765432\quad 1611\quad 82$$
  - ellenőrzés: $$3214282912345698765432161182 \bmod 97 = 1$$
-}
-- 1. Segédfüggvény: Karakter átalakítása számmá (A -> 10, B -> 11...)
charToVal :: Char -> String
charToVal c
    | isDigit c = [c]
    | otherwise = show (fromEnum c - fromEnum 'A' + 10)

-- 2. IBAN ellenőrző logika
isValidIban :: [(String, Int)] -> String -> Bool
isValidIban lengths iban
    | not (all (\c -> isAlphaNum c) iban) = False -- Csak szám és betű
    | length iban /= expectedLen = False         -- Rossz hossz
    | otherwise = (read substituted :: Integer) `mod` 97 == 1
  where
    country = take 2 iban
    -- Megkeressük a listában az ország kódját
    expectedLen = maybe 0 id (lookup country lengths)
    -- Átcsoportosítás: Első 4 karakter a végére
    rearranged = drop 4 iban ++ take 4 iban
    -- Helyettesítés: Minden karaktert számmá alakítunk
    substituted = concatMap charToVal rearranged

-- Főprogram az IBAN-hoz
task2 :: IO ()
task2 = do
    -- Beolvassuk a hosszokat (PL: "RO 24")
    lenContent <- readFile "ibanLength.txt"
    let lengths = map ((\[c, l] -> (c, read l)) . words) (lines lenContent)
    
    -- Beolvassuk az IBAN-okat és rendezzük
    ibanContent <- readFile "iban.txt"
    let ibans = lines ibanContent
    
    -- Megfelelőek kiválogatása
    let validIbans = filter (isValidIban lengths) ibans
    writeFile "okIban.txt" (unlines validIbans)

{-III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva: vezetéknév, keresztnév, születési dátum.
 Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból és állapítsuk meg mindegyik személyről, 
 hogy a hét milyen napján született és mikor van a névnapja. A névnapok megállapításához használhatjuk 
 a [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt) tartalmazó szövegállományt.
-}
data Datum = Datum {
  nap :: Int,
  honap:: Int,
  ev :: Int
} deriving (Show)

data Szemely = Szemely {
  vnev :: [Char],
  knev :: [Char],
  szdatum :: Datum
} deriving (Show)

type NevnapRecord = (String, String)

-- Zeller-féle kongruencia a hét napjának kiszámítására
milyenNap :: Datum -> String
milyenNap (Datum d m y) =
    let y' = if m < 3 then y - 1 else y
        m' = if m < 3 then m + 12 else m
        k = y' `mod` 100
        j = y' `div` 100
        h = (d + ((13 * (m' + 1)) `div` 5) + k + (k `div` 4) + (j `div` 4) - (2 * j)) `mod` 7
        napok = ["Szombat", "Vasárnap", "Hétfő", "Kedd", "Szerda", "Csütörtök", "Péntek"]
    in napok !! h

-- Személyek beolvasása (Feltételezett formátum: Vezetéknév Keresztnév Év Hónap Nap)
parseSzemely :: String -> Szemely
parseSzemely line =
    let parts = words line
        v = parts !! 0
        k = parts !! 1
        y = read (parts !! 2) :: Int
        m = read (parts !! 3) :: Int
        d = read (parts !! 4) :: Int
    in Szemely v k (Datum d m y)

-- Névnapok beolvasása a te fájlod struktúrája alapján
parseNevnap :: String -> NevnapRecord
parseNevnap line =
    let -- A név az első szóközig, zárójelig vagy kettőspontig tart
        nev = takeWhile (\c -> c `notElem` " (:\t") line
        
        -- Megkeressük a zárójelet
        maradek = dropWhile (/= '(') line
        
        -- Ha van zárójel, kivesszük a tartalmát, különben "Nincs megadva dátum" lesz
        datum = if null maradek 
                then "Nincs megadva dátum a fájlban" 
                else takeWhile (/= ')') (tail maradek)
    in (nev, datum)

-- Segédfüggvény: kiszűri a felesleges sorokat (pl. üres sorok, vagy csak egy "A " betű)
isValidNevnapLine :: String -> Bool
isValidNevnapLine line = length line > 2 && any (`elem` ['a'..'z']) line

-- Keresés a névnapok listájában
keresNevnap :: String -> [NevnapRecord] -> String
keresNevnap keresettNev nevnapok =
    case filter (\(n, _) -> n == keresettNev) nevnapok of
        [] -> "Nem található adat a fájlban"
        ((_, d):_) -> d

-- Főprogram
main :: IO ()
main = do
    -- 1. Személyek beolvasása
    szemelyekStr <- readFile "szemelyek.txt"
    -- Kiszűrjük az üres sorokat a személyeknél is a biztonság kedvéért
    let szemelyekLines = filter (\l -> length l > 5) (lines szemelyekStr)
    let szemelyek = map parseSzemely szemelyekLines

    -- 2. Névnapok beolvasása
    nevnapokStr <- readFile "nevnapok.txt"
    -- Kiszűrjük a hibás/üres sorokat, majd feldolgozzuk
    let nevnapLines = filter isValidNevnapLine (lines nevnapokStr)
    let nevnapok = map parseNevnap nevnapLines

    -- 3. Eredmények kiírása
    mapM_ (\sz -> do
        let hetNapja = milyenNap (szdatum sz)
            nnap = keresNevnap (knev sz) nevnapok
        
        putStrLn $ vnev sz ++ " " ++ knev sz ++ ":"
        putStrLn $ "  - Született: " ++ hetNapja
        putStrLn $ "  - Névnapja(i): " ++ nnap
        putStrLn "-----------------------------------"
        ) szemelyek