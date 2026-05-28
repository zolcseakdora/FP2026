import Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing, Down(..))
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO
--1 feladat 
data Diak = Diak
    { nev :: String
    , szak :: String
    , evfolyam :: Int
    , atlag :: Double
    , kreditszam :: Int
    } deriving (Show, Eq)

-- CSV sor értelmezése hibakezeléssel
parseLine :: String -> Either String Diak
parseLine line = case splitOn "," line of
    [n, sz, evStr, atlStr, krStr] ->
        case (readMaybe evStr, readMaybe atlStr, readMaybe krStr) of
            (Just ev, Just atl, Just kr) -> Right $ Diak n sz ev atl kr
            _ -> Left $ "Hibas szamformatum a sorban: " ++ line
    _ -> Left $ "Hibas oszlopszam a sorban: " ++ line

-- Teljes CSV feldolgozása
parseCSV :: String -> Either String [Diak]
parseCSV content = case lines content of
    [] -> Left "Ures fajl"
    (_:rest) -> mapM parseLine rest -- Az első sor a fejléc, elhagyjuk

-- a. Szak szerinti műveletek
szakMuveletek :: [Diak] -> String -> IO ()
szakMuveletek diakok keresettSzak = do
    let szurt = filter (\d -> szak d == keresettSzak) diakok
    putStrLn $ "\nAz " ++ keresettSzak ++ " szakhoz tartozo diakok:"
    mapM_ (\d -> printf "%s - %d - %.2f - %d\n" (nev d) (evfolyam d) (atlag d) (kreditszam d)) szurt
    
    case szurt of
        [] -> putStrLn "Nincsenek diakok ezen a szakon."
        _  -> do
            let atl = sum (map atlag szurt) / fromIntegral (length szurt)
            printf "Az %s szak diakjainak atlaga %.4f\n" keresettSzak atl
            
            let legjobb = maximumBy (comparing atlag) szurt
            printf "Az %s szak legjobb atlaggal rendelkezo diakja %s, atlaga %.2f.\n" keresettSzak (nev legjobb) (atlag legjobb)

-- b. Évfolyam statisztika
evfolyamStatisztika :: [Diak] -> IO ()
evfolyamStatisztika diakok = do
    let csoportositott = groupBy (\d1 d2 -> evfolyam d1 == evfolyam d2) $ sortBy (comparing evfolyam) diakok
    putStrLn "\nEvfolyam statisztikak:"
    mapM_ kiirCsoport csoportositott
  where
    kiirCsoport csoport = do
        let evf = evfolyam (head csoport)
            db = length csoport
            atl = sum (map atlag csoport) / fromIntegral db
        printf "%d. evfolyam: %d diak, atlag: %.1f\n" evf db atl

-- Főprogram
main :: IO ()
main = do
    tartalom <- readFile "diakok.csv"
    case parseCSV tartalom of
        Left hiba -> putStrLn $ "Hiba a beolvasaskor: " ++ hiba
        Right diakok -> do
            -- a. Szak bekérése és feldolgozása
            putStr "Adjon meg egy szakot: "
            hFlush stdout
            bekertSzak <- getLine
            szakMuveletek diakok bekertSzak
            
            -- b. Évfolyam statisztika
            evfolyamStatisztika diakok
            
            -- c. Kitűnők fájlba írása
            let kitunok = filter (\d -> atlag d > 9.0 && kreditszam d >= 30) diakok
            let kitunoSzoveg = unlines $ map show kitunok
            writeFile "kituno.txt" kitunoSzoveg
            
            -- d. Rendezett.csv
            let rendezett = sortBy (\d1 d2 -> case compare (Down $ atlag d1) (Down $ atlag d2) of
                                                EQ -> compare (nev d1) (nev d2)
                                                mas -> mas) diakok
            let csvSor d = intercalate "," [nev d, szak d, show (evfolyam d), show (atlag d), show (kreditszam d)]
            writeFile "rendezett.csv" $ "nev,szak,evfolyam,atlag,kreditszam\n" ++ unlines (map csvSor rendezett)
            
            -- e, f. Legjobb/Legrosszabb és Kredit Min/Max
            let legjobb = maximumBy (comparing atlag) diakok
            let legrosszabb = minimumBy (comparing atlag) diakok
            let maxKredit = maximumBy (comparing kreditszam) diakok
            let minKredit = minimumBy (comparing kreditszam) diakok
            
            printf "\nLegjobb atlag: %s (%.2f), Legrosszabb atlag: %s (%.2f)\n" (nev legjobb) (atlag legjobb) (nev legrosszabb) (atlag legrosszabb)
            printf "Legtobb kredit: %s (%d), Legkevesebb kredit: %s (%d)\n" (nev maxKredit) (kreditszam maxKredit) (nev minKredit) (kreditszam minKredit)
            
            -- h, i. Legtöbb diák és legmagasabb átlagú szak
            let szakSzerint = groupBy (\d1 d2 -> szak d1 == szak d2) $ sortBy (comparing szak) diakok
            let legnepesebb = head $ head $ sortBy (comparing (Down . length)) szakSzerint
            let szakAtlagok = map (\cs -> (szak (head cs), sum (map atlag cs) / fromIntegral (length cs))) szakSzerint
            let legjobbSzak = fst $ maximumBy (comparing snd) szakAtlagok
            
            printf "Legtobb diak a(z) %s szakon van.\n" (szak legnepesebb)
            printf "Legmagasabb atlagu szak: %s\n" legjobbSzak
--2 feladat
data Film = Film
    { cim :: String
    , rendezo :: String
    , mufaj :: String
    , ev :: Int
    , ertekeles :: Double
    , szineszek :: [String]
    } deriving (Show)

-- Egyetlen sor feldolgozása Either-es hibakezeléssel
parseFilmLine :: String -> Either String Film
parseFilmLine sor = case splitOn "," sor of
    -- A CSV oszlopai sorrendben: cim, rendezo, mufaj, ev, ertekeles, szineszek
    [c, r, m, evStr, ertStr, szineszStr] ->
        case (readMaybe evStr, readMaybe ertStr) of
            (Just e, Just ert) -> 
                -- A színészeket a pontosvessző mentén választjuk szét
                let szineszLista = splitOn ";" szineszStr
                in Right $ Film c r m e ert szineszLista
            _ -> Left $ "Hibas szamformatum az ev vagy ertekeles mezoben: " ++ sor
    _ -> Left $ "Hibas oszlopszam a sorban: " ++ sor

-- A teljes fájl beolvasása (az első sor a fejléc, azt eldobjuk)
parseFilmekCSV :: String -> Either String [Film]
parseFilmekCSV tartalom = case lines tartalom of
    [] -> Left "Ures a fajl!"
    (_:adatSorok) -> mapM parseFilmLine adatSorok

-- A feldolgozó függvény ugyanaz marad!
mufajMuveletek :: [Film] -> String -> IO ()
mufajMuveletek lista keresettMufaj = do
    let szurt = filter (\f -> mufaj f == keresettMufaj) lista
    putStrLn $ "\nA " ++ keresettMufaj ++ " mufaju filmek:"
    mapM_ (\f -> printf "%s (%d) rendezte %s\n" (cim f) (ev f) (rendezo f)) szurt
    
    case szurt of
        [] -> putStrLn "Nincs ilyen mufaju film."
        _  -> do
            let atl = sum (map ertekeles szurt) / fromIntegral (length szurt)
            printf "A %s atlag ertekelese %.3f.\n" keresettMufaj atl
            
            let legjobb = maximumBy (comparing ertekeles) szurt
            printf "A %s mufaj legjobban ertekelt muve %s (%d), amit %s rendezett, az ertekeles amit kapott %.1f.\n" 
                keresettMufaj (cim legjobb) (ev legjobb) (rendezo legjobb) (ertekeles legjobb)

-- Főprogram
main :: IO ()
main = do
    tartalom <- readFile "filmek.csv"
    case parseFilmekCSV tartalom of
        Left hiba -> putStrLn $ "Hiba a CSV olvasasakor: " ++ hiba
        Right filmLista -> do
            putStrLn "Sikeres beolvasas!"
            
            -- Innentől kezdve ugyanazokat a lista-műveleteket tudod használni, 
            -- mint amit a korábbi kódban leírtam a 2-es feladathoz.
            
            -- Példa: b. Műfaj beolvasása
            putStr "Adjon meg egy mufajt: "
            bekertMufaj <- getLine
            mufajMuveletek filmLista bekertMufaj

-- 3 feladat 
data Dal = Dal { cim :: String, eloado :: String, album :: String, hossz :: Int, mufaj :: String, ev :: Int } deriving (Show)

parseDal :: String -> Either String Dal
parseDal sor = case splitOn "," sor of
    [c, e, a, hStr, m, evStr] -> 
        case (readMaybe hStr, readMaybe evStr) of
            (Just h, Just ev) -> Right $ Dal c e a h m ev
            _ -> Left $ "Hibas szamformatum: " ++ sor
    _ -> Left $ "Hibas oszlopszam: " ++ sor

parseZenekCSV :: String -> Either String [Dal]
parseZenekCSV tartalom = case lines tartalom of
    [] -> Left "Ures fajl"
    (_:sorok) -> mapM parseDal sorok

main3 :: IO ()
main3 = do
    tartalom <- readFile "zenek.csv"
    case parseZenekCSV tartalom of
        Left hiba -> putStrLn hiba
        Right dalok -> do
            -- a. Rendezzük hossz szerint
            let hosszSzerint = sortBy (comparing hossz) dalok
            putStrLn "\nDalok hossz szerint:"
            mapM_ (\d -> printf "%s - %s (%d mp)\n" (cim d) (eloado d) (hossz d)) hosszSzerint
            
            -- b. Előadó bekérése
            putStr "\nAdjon meg egy eloadot: "
            bekertEloado <- getLine
            let eloadoDalai = filter (\d -> eloado d == bekertEloado) dalok
            putStrLn $ bekertEloado ++ " dalai:"
            mapM_ (\d -> putStrLn $ " - " ++ cim d) eloadoDalai
            let osszPerc = fromIntegral (sum $ map hossz eloadoDalai) / 60.0 :: Double
            printf "Osszesitett jatekidok: %.2f perc\n" osszPerc
            
            -- c. Műfaj statisztika
            let mufajok = groupBy (\d1 d2 -> mufaj d1 == mufaj d2) $ sortBy (comparing mufaj) dalok
            let mufajDb = map (\cs -> (mufaj $ head cs, length cs)) mufajok
            let maxMufaj = maximumBy (comparing snd) mufajDb
            let minMufaj = minimumBy (comparing snd) mufajDb
            printf "\nLegtobb dal: %s (%d), Legkevesebb dal: %s (%d)\n" (fst maxMufaj) (snd maxMufaj) (fst minMufaj) (snd minMufaj)
            
            -- e. Évtized statisztika
            let evtized d = (ev d `div` 10) * 10
            let evtizedek = groupBy (\d1 d2 -> evtized d1 == evtized d2) $ sortBy (comparing evtized) dalok
            putStrLn "\nStatisztika evtizedenkent:"
            mapM_ (\cs -> printf "%ds evek: %d dal\n" (evtized $ head cs) (length cs)) evtizedek
            