import Text.Parsec.Token (GenLanguageDef(reservedNames), GenTokenParser (reserved, natural))
import Foreign (Storable(peekElemOff))

--I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

--egy szám számjegyeinek szorzatát (2 módszerrel)
szjSzorzat 0 = 1
szjSzorzat n = mod n 10 * szjSzorzat (div n 10 )

szjSzorzat2 n
    | n < 0 = error "neg. szam"
    | div n 10 == 0 = n
    | otherwise = mod n 10 * szjSzorzat2 (div n 10)

ls1 = [234, 64, 12, 9, 0]
szjSzorzatLs ls = map szjSzorzat2 ls
szjSzorzatLs2 ls = map(\x -> ( x, szjSzorzat2 x)) ls
szjSzorzat3 n res
    | n < 0 = error "neg. szam"
    | div n 10 == 0 = res
    | otherwise = szjSzorzat3 (div n 10 ) (res * (mod n 10))

-- egy szám számjegyeinek összegét (2 módszerrel)
szjOsszeg n
  | n < 0 = szjOsszeg( abs n )
  | n < 10 = n
  | otherwise = mod n 10 + szjOsszeg( div n 10 )

szjOsszeg2 n res
  | n < 0 = szjOsszeg2( abs n ) res
  | n < 10 = res + n
  | otherwise  = szjOsszeg2( div n 10 ) (res + mod n 10)

szjOsszegLs = map szjOsszeg ls1

szjOsszegLs2 ls = map(\x -> szjOsszeg2 x 0) ls

-- egy szám számjegyeinek számát (2 módszerrel)
szjSzam n res 
  | n < 0 = szjSzam (abs n ) res
  | n < 10 = res + 1
  | otherwise = szjSzam ( div n 10) (res +1)

szjSzam2 n
  | n < 0 = szjSzam2 (abs n ) 
  | n < 10 = 1
  | otherwise = 1 + szjSzam2 ( div n 10)

szjSzamLs ls = map szjSzam2 ls

-- egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, 
--a következő eredményt kell kapjuk:
--```haskell
-- > fugv4 577723707 7
-- 35
--  ```
szjSzamOsszeg n szj
  | szj > 9 = error "nem szj"
  | n < 10 = if n == szj then szj else 0
  | otherwise = if mod n 10 == szj then szj + szjSzamOsszeg( div n 10 ) szj else szjSzamOsszeg( div n 10 ) szj 

szjSzamOsszeg2 n szj elof
  | szj > 9 = error "nem szj"
  | n < 10 = if n == szj then (elof + 1) * szj else elof * szj
  | otherwise  = if mod n 10 == szj then szjSzamOsszeg2( div n 10 ) szj ( elof + 1) else szjSzamOsszeg2( div n 10 ) szj elof

ls2 = [(577723707, 7), (423, 3), (0, 1), (12, 2)]
szjSzamOsszegLs ls = map (uncurry szjSzamOsszeg) ls
szjSzamOsszegLs2 ls = map (\(x,szj) -> szjSzamOsszeg x szj) ls
-- egy szám páros számjegyeinek számát
parosSzamSzj n
  | n < 0 = parosSzamSzj ( abs n)
  | n < 10 = if even n then 1 else 0
  | otherwise = if even (mod n 10) then 1 + parosSzamSzj( div n 10) else parosSzamSzj(div n 10)

parosSzamSzj2 n res
  | n < 0 = parosSzamSzj2 ( abs n) res
  | n < 10 = if even n then res + 1 else res
  | otherwise = if even (mod n 10) then parosSzamSzj2( div n 10) (res +1) else parosSzamSzj2(div n 10) res

-- egy szám legnagyobb számjegyét
lgSzj n ln
  | n < 0 = lgSzj ( abs n ) ln
  | n < 10 = max n ln 
  | otherwise = if mod n 10 > ln then lgSzj ( div n 10 ) (mod n 10) else lgSzj(div n 10) ln 

lgSzjLs ls = map (\x -> lgSzj x 0) ls

-- egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma)
 {- 
 Példák függvényhívásokra:

  ```haskell
  fugv 7673573 10 7 -> 3
  fugv 1024 2 1 -> 1
  fugv 1023 2 1 -> 10
  fugv 345281 16 4 -> 2
   ```
 -}

bSzamrDSzj n b d 
  | n < 0 = bSzamrDSzj ( abs n) b d
  | n < b = if n == d then 1 else 0
  | otherwise = if mod n b == d then 1 + bSzamrDSzj (div n b) b d else bSzamrDSzj( div n b ) b d

ls3 = [(7673573, 10, 7), (1024, 2, 1), (1023, 2, 1), (345281, 16, 4)]

bSzamrDSzjLs ls = map (\ (n,b,d) -> bSzamrDSzj n b d ) ls3
-- az 1000-ik Fibonacci számot

fibo a b res n 
  | n == 0 = res
  | otherwise = fibo b res (res+b) (n-1)

fiboN n = fibo 0 1 0 n

fiboN2 n = fiboSg 0 1 0 n
  where
    fiboSg _ _ res 0 = res
    fiboSg a b res n = fiboSg b res ( res + b) (n - 1)



{-
II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

**Megoldott feladatok:**

- Határozzuk meg egy szám számjegyeinek összegét:
  I. módszer:

  ```haskell
  szOsszeg :: Int -> Int
  szOsszeg 0 = 0
  szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

  > szOsszeg 123
  ```

  II. módszer:

  ```haskell
  szOsszeg1 :: Int -> Int -> Int
  szOsszeg1 0 t = t
  szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

  > szOsszeg1 123 0
  -}
