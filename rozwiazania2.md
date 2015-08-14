### Zadanie
Stworzyć własny typ danych, który reprezentuje kolory

```haskell
data RGB = RGB (Int, Int, Int)
data Kanały = Kanały { czerwony::Int, zielony::Int, niebieski::Int }
data Kolor = Czerwony | Zółty | Zielony | Cyjan | Niebieski | Fuksja
```

### Zadanie
Zaimplementować operacją dodawania z klasy `Num` dla tego nowego typu przechowującego kolory

```haskell
x +% y = (x + y) `mod` 256
infixl 6 +%

instance Num RGB where
    (RGB (r1, g1, b1)) + (RGB (r2, g2, b2)) = RGB (r1 +% r2, g1 +% g2, b1 +% b2)

instance Num Kanały where
    p + q = Kanały (czerwony p +% czerwony q) (zielony p +% zielony q) (niebieski p +% niebieski q)

instance Num Kolor where

```

### Zadanie
Mając typ danych `data Drzewo a = Nic | Węzeł (Drzewo a) (Drzewo a)` zaimplementować dla niego instancję funktora

```haskell

```

### Zadanie
Bazując na poprzedniej implementacji, zapisać dla typu `Drzewo` instancję aplikatora

```haskell

```
