### Zadanie
Stworzyć własny typ danych, który reprezentuje kolory

```haskell
data RGB = RGB (Int, Int, Int)
data Kanały = RGB { czerwony::Int, zielony::Int, niebieski::Int }
data Kolor = Czerwony | Zółty | Zielony | Cyjan | Niebieski | Fuksja
```

### Zadanie
Zaimplementować operacją dodawania z klasy `Num` dla tego nowego typu przechowującego kolory

```haskell
instance Num Kolor where
    
instance Num RGB where
    
data Kanały = RGB { czerwony::Int, zielony::Int, niebieski::Int }
```

### Zadanie
Mając typ danych `data Drzewo a = Nic | Węzeł (Drzewo a) (Drzewo a)` zaimplementować dla niego instancję funktora

```haskell

```

### Zadanie
Bazując na poprzedniej implementacji, zapisać dla typu `Drzewo` instancję aplikatora

```haskell

```
