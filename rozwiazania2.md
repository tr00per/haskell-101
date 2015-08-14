### Zadanie
Stworzyć własny typ danych, który reprezentuje kolory

```haskell
data Kolor = Czerwony | Zółty | Zielony | Cyjan | Niebieski | Fuksja
data RGB = RGB (Int, Int, Int)
data Kanały = RGB { czerwony::Int, zielony::Int, niebieski::Int }
```

### Zadanie
Zaimplementować klasę `Num` dla tego nowego typu przechowującego kolory

```haskell

```

### Zadanie
Mając typ danych `data Drzewo a = Nic | Węzeł (Drzewo a) (Drzewo a)` zaimplementować dla niego instancję funktora

```haskell

```

### Zadanie
Bazując na poprzedniej implementacji, zapisać dla typu `Drzewo` instancję aplikatora

```haskell

```
