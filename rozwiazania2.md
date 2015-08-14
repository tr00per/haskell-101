### Rozwiązanie
Stworzyć własny typ danych, który reprezentuje kolory

```haskell
data RGB = RGB (Int, Int, Int)
data Kanały = Kanały { czerwony::Int, zielony::Int, niebieski::Int }
data Kolor = Czerwony | Zółty | Zielony | Cyjan | Niebieski | Fuksja
```

![](https://i.chzbgr.com/maxW500/8547829760/hFEDF9230/)

### Rozwiązanie
Zaimplementować operacją dodawania z klasy `Num` dla tego nowego typu przechowującego kolory

```haskell
x +% y = (x + y) `mod` 256
infixl 6 +%

instance Num RGB where
    (RGB (r1, g1, b1)) + (RGB (r2, g2, b2)) =
        RGB (r1 +% r2, g1 +% g2, b1 +% b2)

instance Num Kanały where
    p + q = Kanały
                (czerwony p +% czerwony q)
                (zielony p +% zielony q)
                (niebieski p +% niebieski q)

instance Num Kolor where

```

![](https://i.chzbgr.com/maxW500/8548211712/hF0537D89/)

### Rozwiązanie
Mając typ danych `data Drzewo a = Nic | Węzeł a (Drzewo a) (Drzewo a)` zaimplementować dla niego instancję funktora

```haskell
data Drzewo a = Nic | Węzeł a (Drzewo a) (Drzewo a)

instance Functor (Drzewo a) where
    fmap _ Nic = Nic
    fmap f 
```

![](https://i.chzbgr.com/maxW500/8549208320/hD46DDB7B/)

### Rozwiązanie
Bazując na poprzedniej implementacji, zapisać dla typu `Drzewo` instancję aplikatora

```haskell


```

![](https://i.chzbgr.com/maxW500/8431718656/h1E8B69B5/)
