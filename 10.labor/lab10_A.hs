{- 10. labor

I. A [jelszavakNevek.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/jelszavakNevek.txt) állományban nevek és lementett jelszavak hash értéke
 van. A jelszavak hash értékei [Diviánszky Péter jelszavas feladata](http://lambda.inf.elte.hu/Password.xml) alapján voltak létrehozva. 
 Tudva azt, hogy a következő kulcsképző függvények valamelyikével normalizáltuk a felhasználó jelszavát, írjunk Haskell programot, 
 amely meghatározza, hogy kinek a jelszava a LLEKSAH, illetve a masPSWD123:

- kulcsképző függvény I. (jelszó ++ salt + forditott jelszo)
- kulcsképző függvény II, ahol a lehetséges hosszak $$16,\ 32,\ 64$$.
-}
import System.IO
import Password
import Roman
import Data.List
import Data.Ord

-- Előállítja az összes lehetséges KDF-et a feladatleírás alapján
-- KDF1 és KDF2 a megadott 16, 32, 64-es hosszakra
lehetsegesKDFs :: [KDF]
lehetsegesKDFs = [ kdf1, kdf2 16, kdf2 32, kdf2 64 ]

-- Megvizsgál egyetlen embert a fájlból
keresJelszot :: String -> String -> [KDF] -> [String] -> Maybe String
keresJelszot nev sor2 kdfs keresettJelszavak =
    -- Beolvassuk a jelszót a megadott minta ábécé és a '$' elválasztó alapján
    case parsePassword alphabet '$' sor2 of
        Nothing -> Nothing
        Just pwdObj -> 
            -- Végigpróbáljuk a megadott tiszta jelszavakat (LLEKSAH, masPSWD123)
            let talalatok = [ pwd | pwd <- keresettJelszavak, checkPassword pwdObj kdfs pwd ]
            in case talalatok of
                (t:_) -> Just (nev ++ " jelszava a(z): " ++ t)
                []    -> Nothing

-- A lista kettesével történő feldolgozása (Név és Hash$Salt sorok párosítása)
feldolgozAdatok :: [String] -> [String]
feldolgozAdatok [] = []
feldolgozAdatok (sor1:sor2:maradek)
    | null sor1 = feldolgozAdatok (sor2:maradek) -- Üres sor átugrása
    | null sor2 = feldolgozAdatok maradek
    | otherwise = 
        let eredmeny = keresJelszot sor1 sor2 lehetsegesKDFs ["LLEKSAH", "masPSWD123"]
        in case eredmeny of
            Just e  -> e : feldolgozAdatok maradek
            Nothing -> feldolgozAdatok maradek
feldolgozAdatok _ = []

-- Főprogram
main :: IO ()
main = do
    tartalom <- readFile "jelszavakNevek.txt"
    let sorok = lines tartalom
        talalatok = feldolgozAdatok sorok
    
    putStrLn "A keresett jelszavak tulajdonosai:"
    if null talalatok
        then putStrLn "  - Nem található egyezés a megadott jelszavakra."
        else mapM_ (\t -> putStrLn $ "  - " ++ t) talalatok

{-
II. Írjunk egy-egy Haskell programot, amely

- meghatározza a billentyűzetről beolvasott két római szám összegét, szorzatát, különbségét, osztási hányadosát. Az eredményt is római 
számokként adjuk meg. A számításokat végezhetjük arabszámokkal,
- kiírja egy állományba 1-től 3999-ig az összes arab számot és a számoknak megfelelo római számot,
- egy szövegállományban található személynevek és a hozzájuk tartozó születési időpontok alapján meghatározza a születési időpontok 
római számokbeli alakját, és az eredményt a megfelelő nevekkel együtt átírja egy másik szövegállományba.
-}

main1 = do
    hSetBuffering stdout NoBuffering -- Azonnali kiírás a konzolra
    
    putStr "Adja meg az elso romai szamot: "
    r1 <- getLine
    putStr "Adja meg a masodik romai szamot: "
    r2 <- getLine
    
    let a1 = romanToArab r1
        a2 = romanToArab r2
    
    putStrLn "\n--- Eredmenyek ---"
    putStrLn $ r1 ++ " + " ++ r2 ++ " = " ++ arabToRoman (a1 + a2)
    putStrLn $ r1 ++ " * " ++ r2 ++ " = " ++ arabToRoman (a1 * a2)
    
    -- Kivonás ellenőrzése (római számoknál nincs negatív vagy nulla)
    putStr "Kivonas: "
    if a1 > a2 
        then putStrLn $ r1 ++ " - " ++ r2 ++ " = " ++ arabToRoman (a1 - a2)
        else putStrLn "Nem ertelmezett (az elso szamnak nagyobbnak kell lennie)"
        
    -- Osztás ellenőrzése (római számoknál nincs nulla vagy tört)
    putStr "Osztas: "
    if a2 == 0 
        then putStrLn "Nullaval nem lehet osztani!"
        else let hanyados = a1 `div` a2
             in if hanyados > 0 
                then putStrLn $ r1 ++ " / " ++ r2 ++ " = " ++ arabToRoman hanyados
                else putStrLn "Az eredmeny kisebb mint I (0 vagy tort)"

main2 = do
    -- Legyártjuk a sorokat: "ArabSzám -> RómaiSzám" formátumban
    let sorok = [ show i ++ " -> " ++ arabToRoman i | i <- [1..3999] ]
        tartalom = unlines sorok
        
    writeFile "tablazat.txt" tartalom
    putStrLn "A 'tablazat.txt' sikeresen letrejon 1-tol 3999-ig!"

-- Egyetlen sor feldolgozása
-- Példa: "Kovacs Janos 1990" -> "Kovacs Janos MCMXC"
feldolgozSor :: String -> String
feldolgozSor sor =
    let reszek = words sor
    in if null reszek
       then ""
       else let nev = unwords (init reszek)       -- Minden szó, kivéve az utolsó
                arabEv = read (last reszek) :: Int -- Az utolsó szó az évszám
                romaiEv = arabToRoman arabEv
            in nev ++ " " ++ romaiEv

main3 = do
    -- 1. Beolvasás
    tartalom <- readFile "szuletesek.txt"
    let sorok = lines tartalom
    
    -- 2. Átalakítás
    let atalakitottSorok = map feldolgozSor (filter (not . null) sorok)
        ujTartalom = unlines atalakitottSorok
        
    -- 3. Kiírás az új fájlba
    writeFile "szuletesek_romai.txt" ujTartalom
    putStrLn "Az adatok sikeresen atirva a 'szuletesek_romai.txt' allomanyba!"

{-
III. Egy Fesztivalok típusú listában a következő adatok vannak eltárolva: fesztiválnév, fesztiválkód, jegyár, és az együttesnevek, 
azaz adott a következő adatszerkezet:
Írjunk egy Haskell programot, amely egy Fesztivalok típusú lista esetében:

- meghatározza azt a fesztivált, ahol a legtöbb együttes lép fel,
- meghatározza, minden egyes fesztivál esetében, a résztvevő együttesek számát,
- kiírja formázva, a jegyárak alapján rendezve, a fesztiválok adatait,
- létrehoz egy bináris keresőfát, a fesztiválnév alapján, majd inorder bejárást alkalmazva, meghatározza a fesztiválok ábécé sorrendjét.
-}
data Fesztivalok = Fesztivalok {
  fFesztival :: String,
  fKod :: Int,
  fAr :: Int,
  fEgyuttes :: [String]
} deriving (Show)

legtobbEgyuttes :: [Fesztivalok] -> Fesztivalok
legtobbEgyuttes fesztivalok = maximumBy (comparing (length . fEgyuttes)) fesztivalok

egyuttesekSzama :: [Fesztivalok] -> [(String, Int)]
egyuttesekSzama fesztivalok = [ (fFesztival f, length (fEgyuttes f)) | f <- fesztivalok ]

rendezEsFormaz :: [Fesztivalok] -> IO ()
rendezEsFormaz fesztivalok = do
    let rendezett = sortBy (comparing fAr) fesztivalok
    putStrLn "Fesztiválok a jegyár alapján rendezve:"
    mapM_ (\f -> putStrLn $ "  - " ++ fFesztival f ++ " (Kód: " ++ show (fKod f) ++ ") | Ár: " ++ show (fAr f) ++ " Ft") rendezett

data KeresoFa = Ures | Csomopont Fesztivalok KeresoFa KeresoFa

-- JAVÍTÁS: A nagybetűs 'Fesztivalok' szót töröltük, és tiszta, kétargumentumos mintákat használunk
beszur :: Fesztivalok -> KeresoFa -> KeresoFa
beszur uj Ures = Csomopont uj Ures Ures
beszur uj (Csomopont gyoker bal jobb)
    | fFesztival uj < fFesztival gyoker = Csomopont gyoker (beszur uj bal) jobb
    | otherwise                         = Csomopont gyoker bal (beszur uj jobb)

-- Lista átalakítása bináris fává (hajtogatással)
faEpites :: [Fesztivalok] -> KeresoFa
faEpites fesztivalok = foldr beszur Ures fesztivalok

-- Inorder bejárás: Bal részfa -> Gyökér -> Jobb részfa
inorder :: KeresoFa -> [Fesztivalok]
inorder Ures = []
inorder (Csomopont gyoker bal jobb) = inorder bal ++ [gyoker] ++ inorder jobb

-- Ábécé sorrend meghatározása a fa segítségével
abeceSorrend :: [Fesztivalok] -> [String]
abeceSorrend fesztivalok = map fFesztival (inorder (faEpites fesztivalok))

mintaAdat :: [Fesztivalok]
mintaAdat = 
  [ Fesztivalok "Sziget" 101 85000 ["Arctic Monkeys", "Dua Lipa", "Stromae", "Justin Bieber"]
  , Fesztivalok "Untold" 202 92000 ["David Guetta", "Alok", "Kygo"]
  , Fesztivalok "Balaton Sound" 303 72000 ["Marshmello", "Timmy Trumpet", "Martin Garrix", "Alesso", "Dimitri Vegas"]
  , Fesztivalok "Electric Castle" 404 65000 ["Twenty One Pilots", "Gorillaz"]
  ]

main4 :: IO ()
main4 = do
    putStrLn "=== FESZTIVÁLOK ADATFELDOLGOZÁSA ===\n"

    -- 1. Teszt: Legtöbb együttes
    let legszorakoztatobb = legtobbEgyuttes mintaAdat
    putStrLn $ "A legtöbb együttest felvonultató fesztivál: " ++ fFesztival legszorakoztatobb 
             ++ " (" ++ show (length (fEgyuttes legszorakoztatobb)) ++ " együttes)\n"

    -- 2. Teszt: Együttesek száma fesztiválonként
    putStrLn "Együttesek száma fesztiválonként:"
    mapM_ (\(nev, db) -> putStrLn $ "  - " ++ nev ++ ": " ++ show db ++ " db fellépő") (egyuttesekSzama mintaAdat)
    putStrLn ""

    -- 3. Teszt: Árak szerinti formázott kiírás
    rendezEsFormaz mintaAdat
    putStrLn ""

    -- 4. Teszt: Bináris fa alapú ábécé sorrend
    putStrLn "Fesztiválok ábécé sorrendben (Bináris keresőfával meghatározva):"
    mapM_ (\nev -> putStrLn $ "  * " ++ nev) (abeceSorrend mintaAdat)
{-
IV. Egy szövegállományban, egy adott sportolimpiáról a következő adatok vannak eltárolva: ország, és az eredmények sportáganként, ahol az eredmények egy (sportág, érmék száma) értékpárokból álló listát jelent, azaz adott a következő adatszerkezet:

```haskell
data Olimpia = Olimpia {
  oOrszag :: String,
  oSportagak :: [(String, Int)]
} deriving (Show)
```

Írjunk egy Haskell programot, amely az állományban levő adatok alapján létrehoz egy Olimpia típusú listát és

- meghatározza, hogy egy adott ország összesen, hány érmét szerzett,
- meghatározza, hogy melyik ország szerzett a legtöbb érmét a sportolimpián,
- meghatározza, hogy milyen sportágak esetében osztottak díjakat,
- meghatározza, hogy egy adott sportágon belül, hány díjat osztottak,
- egy adott ország esetében kiírja, formázva, a sportáganként szerzett érmék száma szerinti, rendezett sorrendet,
- létrehoz egy bináris keresőfát, az országnevek alapján, majd inorder bejárást alkalmazva kiírja formázva az ábécé sorrendet.

V. Írjunk Haskell függvényt, amely létrehoz egy valós számokat tároló bináris keresőfát, és meghatározza inorder bejárással a számok rendezett sorrendjét, illetve a csomópontokban található számok összegét.
-}