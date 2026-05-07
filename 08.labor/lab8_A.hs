import Data.List
import System.IO
{-8. labor

I. Írjunk egy Haskell programot, amelyben megadunk egy konstans Fesztivalok elemtípusú listát, majd

- határozzuk meg egy adott fesztiválon szereplő eggyütteseket,
- határozzuk meg azokat az együtteseket, amelyek egy adott értéknél olcsóbban árulják koncertjegyeiket,
- határozzuk meg, hogy hány olyan együttes szerepel a listában, amely egy adott értéknél olcsóbban árulja koncertjegyét,
- rendezzük a lista tartalmát az együttesek nevei alapján ábécé sorrendbe (insertSort),
- rendezzük a lista tartalmát a jegyárak szerint csökkenő sorrendbe (qSort),
- rendezzük a lista tartalmát összefésülő rendezéssel a kod értékek alapján,
- határozzuk, meg, hogy agy adott fesztiválon mennyi a jegyek átlagértéke,
- írjuk meg az általános összefésülő, illetve beszúró rendezés algoritmusokat.
-}
data Fesztivalok = Fesztivalok {
        fEgyuttes :: String,
                fFesztival :: String,
                        fAr :: Int,
                                fKod :: Int
                                    } deriving (Show)

                                    fesztivalAdatok = 
                                        [ Fesztivalok "Azahriah" "Sziget" 15000 101
                                            , Fesztivalok "Krubi" "Sziget" 12000 105
                                                , Fesztivalok "Carson Coma" "Fishing" 8000 102
                                                    , Fesztivalok "Pogany Indulo" "Strand" 9000 104
                                                        ]

                                                        egyuttesekFesztivalon nev lista = [fEgyuttes x | x <- lista, fFesztival x == nev]
                                                        olcsobbak limit lista = [fEgyuttes x | x <- lista, fAr x < limit]
                                                        hanyOlcsobb limit lista = length [x | x <- lista, fAr x < limit]

                                                        insertSort [] = []
                                                        insertSort (x:xs) = beszuras x (insertSort xs)
                                                            where
                                                                    beszuras e [] = [e]
                                                                            beszuras e (y:ys) | fEgyuttes e <= fEgyuttes y = e : y : ys
                                                                                                      | otherwise = y : beszuras e ys
                                                                                                       
                                                                                                       qSort [] = []
                                                                                                       qSort (x:xs) = qSort [a | a <- xs, fAr a > fAr x] 
                                                                                                                       ++ [x] ++ 
                                                                                                                                       qSort [a | a <- xs, fAr a <= fAr x]

                                                                                                                                       mSort [] = []
                                                                                                                                       mSort [x] = [x]
                                                                                                                                       mSort xs = fesus (mSort bal) (mSort jobb)
                                                                                                                                           where
                                                                                                                                               (bal, jobb) = splitAt (length xs `div` 2) xs
                                                                                                                                                   fesus [] ys = ys
                                                                                                                                                       fesus xs [] = xs
                                                                                                                                                           fesus (x:xs) (y:ys) | fKod x <= fKod y = x : fesus xs (y:ys)
                                                                                                                                                                                   | otherwise = y : fesus (x:xs) ys                      

                                                                                                                                                                                   atlagAr nev lista = 
                                                                                                                                                                                       let arak = [fAr x | x <- lista, fFesztival x == nev]
                                                                                                                                                                                           in if null arak then 0 
                                                                                                                                                                                                  else fromIntegral (sum arak) / fromIntegral (length arak)

                                                                                                                                                                                                  altalanosInsertSort [] = []
                                                                                                                                                                                                  altalanosInsertSort (x:xs) = beszuras x (altalanosInsertSort xs)
                                                                                                                                                                                                    where
                                                                                                                                                                                                        beszuras e [] = [e]
                                                                                                                                                                                                            beszuras e (y:ys) | e <= y = e : y : ys
                                                                                                                                                                                                                                  | otherwise = y : beszuras e ys

                                                                                                                                                                                                                                  altalanosMergeSort [] = []
                                                                                                                                                                                                                                  altalanosMergeSort [x] = [x]
                                                                                                                                                                                                                                  altalanosMergeSort xs = merge (altalanosMergeSort l) (altalanosMergeSort r)
                                                                                                                                                                                                                                    where
                                                                                                                                                                                                                                        (l, r) = splitAt (length xs `div` 2) xs
                                                                                                                                                                                                                                            merge [] ys = ys
                                                                                                                                                                                                                                                merge xs [] = xs
                                                                                                                                                                                                                                                    merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                                                                                                                                                                                                                                                                            | otherwise = y : merge (x:xs) ys
                                                                                                                                                                                                                                                                            {-II. Egy szövegállományban egy adott városról a következő adatok vannak eltárolva: városnév, népességszám, területméret, azaz adott a következő adatszerkezet:

                                                                                                                                                                                                                                                                            ```haskell
                                                                                                                                                                                                                                                                            data Varos = Varos {
                                                                                                                                                                                                                                                                            vNev :: String,
                                                                                                                                                                                                                                                                            vNepSzam :: Int,
                                                                                                                                                                                                                                                                            vTerMeret :: Int
                                                                                                                                                                                                                                                                            } deriving (Show)
                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                            Írjunk egy Haskell programot, amely az állományban levő adatok alapján létrehoz egy Varos elemtípusú listát, majd

                                                                                                                                                                                                                                                                            - meghatározza, hogy hány olyan város van, amelyiknek a népsűrűsége egy megadott $[a, b]$ intervallumba esik, ahol az $a$ és $b$ értékeket a billentyűzetről olvassuk be,
                                                                                                                                                                                                                                                                            - meghatározza a városok népsűrűség szerinti rendezett sorrendjét, az eredményt elegáns formában kiírva a képernyőre (népsűrűség = népesség-szám / terület-méret),
                                                                                                                                                                                                                                                                            - a népességszám alapján felépít egy bináros keresőfát, alkalmazva a megfelelő bejárási módot kiírja egy állományba a városokra vonatkozó adatokat a népsségszám alapján rendezve, majd a bináris kersőfát használva megállpítja, hogy melyik a legnagyobb, illetve melyik a legkisebb népességszámmal rendelkező város.

                                                                                                                                                                                                                                                                            -}
                                                                                                                                                                                                                                                                            data Varos = Varos {
                                                                                                                                                                                                                                                                                vNev :: String,
                                                                                                                                                                                                                                                                                    vNepSzam :: Int,
                                                                                                                                                                                                                                                                                        vTerMeret :: Int
                                                                                                                                                                                                                                                                                        } deriving (Show,Read)

                                                                                                                                                                                                                                                                                        nepsuruseg v = fromIntegral (vNepSzam v) / fromIntegral (vTerMeret v)

                                                                                                                                                                                                                                                                                        main :: IO ()
                                                                                                                                                                                                                                                                                        main = do
                                                                                                                                                                                                                                                                                            tartalom <- readFile "varosok.txt"
                                                                                                                                                                                                                                                                                                let varosok = map read (lines tartalom) :: [Varos]

                                                                                                                                                                                                                                                                                                    putStrLn "Adja meg az 'a' erteket (min nepsuruseg):"
                                                                                                                                                                                                                                                                                                        aStr <- getLine
                                                                                                                                                                                                                                                                                                            let a = read aStr :: Double
                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                    putStrLn "Adja meg a 'b' erteket (max nepsuruseg):"
                                                                                                                                                                                                                                                                                                                        bStr <- getLine
                                                                                                                                                                                                                                                                                                                            let b = read bStr :: Double

                                                                                                                                                                                                                                                                                                                                let szurtVarosok = [v | v <- varosok, nepsuruseg v >= a, nepsuruseg v <= b]
                                                                                                                                                                                                                                                                                                                                    putStrLn $ "Az intervallumba eso varosok szama: " ++ show (length szurtVarosok)

                                                                                                                                                                                                                                                                                                                                        let rendezettNepsuruseg = sortOn nepsuruseg varosok
                                                                                                                                                                                                                                                                                                                                            putStrLn "\nVarosok nepsuruseg szerinti sorrendben:"
                                                                                                                                                                                                                                                                                                                                                mapM_ (\v -> putStrLn $ "- " ++ vNev v ++ ": " ++ show (nepsuruseg v) ++ " fo/km2") rendezettNepsuruseg

                                                                                                                                                                                                                                                                                                                                                    let fa = faEpit varosok

                                                                                                                                                                                                                                                                                                                                                        let rendezettNepesseg = inorder fa
                                                                                                                                                                                                                                                                                                                                                            writeFile "rendezett_nepesseg.txt" (unlines (map show rendezettNepesseg))
                                                                                                                                                                                                                                                                                                                                                                putStrLn "\nVarosok adatai nepesseg szerint mentve a 'rendezett_nepesseg.txt' fajlba."

                                                                                                                                                                                                                                                                                                                                                                    let legkisebb = legkisebbNep fa
                                                                                                                                                                                                                                                                                                                                                                        let legnagyobb = legnagyobbNep fa
                                                                                                                                                                                                                                                                                                                                                                            putStrLn $ "Legkisebb nepessegu varos: " ++ vNev legkisebb ++ " (" ++ show (vNepSzam legkisebb) ++ " fo)"
                                                                                                                                                                                                                                                                                                                                                                                putStrLn $ "Legnagyobb nepessegu varos: " ++ vNev legnagyobb ++ " (" ++ show (vNepSzam legnagyobb) ++ " fo)"

                                                                                                                                                                                                                                                                                                                                                                                data Fa = Ures | Csomopont Varos Fa Fa

                                                                                                                                                                                                                                                                                                                                                                                beszur Ures v = Csomopont v Ures Ures
                                                                                                                                                                                                                                                                                                                                                                                beszur (Csomopont gy bal jobb) v
                                                                                                                                                                                                                                                                                                                                                                                    | vNepSzam v < vNepSzam gy = Csomopont gy (beszur bal v) jobb
                                                                                                                                                                                                                                                                                                                                                                                        | otherwise                = Csomopont gy bal (beszur jobb v)

                                                                                                                                                                                                                                                                                                                                                                                        faEpit = foldl beszur Ures

                                                                                                                                                                                                                                                                                                                                                                                        inorder Ures = []
                                                                                                                                                                                                                                                                                                                                                                                        inorder (Csomopont v bal jobb) = inorder bal ++ [v] ++ inorder jobb

                                                                                                                                                                                                                                                                                                                                                                                        legkisebbNep (Csomopont v Ures _) = v
                                                                                                                                                                                                                                                                                                                                                                                        legkisebbNep (Csomopont _ bal _)  = legkisebbNep bal

                                                                                                                                                                                                                                                                                                                                                                                        legnagyobbNep (Csomopont v _ Ures) = v
                                                                                                                                                                                                                                                                                                                                                                                        legnagyobbNep (Csomopont _ _ jobb)  = legnagyobbNep jobb

                                                                                                                                                                                                                                                                                                                                                                                        {-
III. Egy listában kriptográfiai algoritmusok parméterei vannak eltárolva. Három fajta kripto algoritmust tárolhat a lista: StreamCipher, BlockCipher, BlockCipherMode. Egy StreamCipher típusú adat paraméterei a következők lehetnek: algoritmus név, kulcs méretek, és protokollok amelyekben használják. Egy BlockCipher típusú adat paraméterei a következők lehetnek: algoritmus név, kulcs méretek, blokkméret, és protokollok amelyekben használják. Egy BlockCipherMode típusú adat paraméterei a következő: algoritmus név. Pontosabban adott a következő adatszerkezet, illetve konstans lista:

```haskell
type Name = String
type KeyLen = [Int]
type BlockLen = Int

type Protocol = String
data Crypto =
StreamCipher Name KeyLen [Protocol]
| BlockCipher Name KeyLen BlockLen [Protocol]
| BlockCipherMode Name
deriving (Show, Read, Eq)

lsCrypto = [
BlockCipher "AES" [128, 192, 256] 128 ["TLS", "PGP", "Kerberos"],
BlockCipherMode "ECB",
BlockCipherMode "CBC",
    BlockCipher "Twofish" [128, 192, 256] 128 ["PGP", "Kerberos"],
    StreamCipher "ChaCha20" [128, 256] ["TLS", "S/MIME", "SSH"],
        BlockCipher "3DES" [168] 64 ["TLS", "PGP", "Kerberos"],
        BlockCipherMode "CTR",
            BlockCipherMode "GCM",
            StreamCipher "RC4" [40..2048] ["Kerberos"]
            ]
            ```

Írjunk egy Haskell-programot, amely a listában levő adatok esetében:

- meghatározza, hogy hány BlockCipherMode típusú adatot tárol a lista,
- kiválogatja a BlockCipherMode típusú adatokat egy külön listába,
- kiválogatja azokat a BlockCipher típusú adatokat amelyek a legtöbb protokollban szerepelnek,
- kiírja a StreamCipher típusú adatokat, név szerint rendezve egy szövegállományba.
            -}