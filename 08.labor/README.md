# 8. labor

I. Írjunk egy Haskell programot, amelyben megadunk egy konstans Fesztivalok elemtípusú listát, majd

- határozzuk meg egy adott fesztiválon szereplő eggyütteseket,
- határozzuk meg azokat az együtteseket, amelyek egy adott értéknél olcsóbban árulják koncertjegyeiket,
- határozzuk meg, hogy hány olyan együttes szerepel a listában, amely egy adott értéknél olcsóbban árulja koncertjegyét,
- rendezzük a lista tartalmát az együttesek nevei alapján ábécé sorrendbe (insertSort),
- rendezzük a lista tartalmát a jegyárak szerint csökkenő sorrendbe (qSort),
- rendezzük a lista tartalmát összefésülő rendezéssel a kod értékek alapján,
- határozzuk, meg, hogy agy adott fesztiválon mennyi a jegyek átlagértéke,
- írjuk meg az általános összefésülő, illetve beszúró rendezés algoritmusokat.

  ```haskell
    data Fesztivalok = Fesztivalok {
        fEgyuttes :: String,
            fFesztival :: String,
                fAr :: Int,
                    fKod :: Int
                      } deriving (Show)
                        ```

                        II. Egy szövegállományban egy adott városról a következő adatok vannak eltárolva: városnév, népességszám, területméret, azaz adott a következő adatszerkezet:

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
                                                        