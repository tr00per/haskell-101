### Rozwiązanie
Stworzyć własny typ danych, który reprezentuje kolory

```haskell
data RGB = RGB (Int, Int, Int)

data Kanały = Kanały { czerwony::Int, zielony::Int, niebieski::Int }

data Kolor = Czerwony | Żółty | Zielony | Cyjan |
             Niebieski | Fuksja | Biały | Czarny
```

![](https://i.chzbgr.com/maxW500/8547829760/hFEDF9230/)

### Rozwiązanie
Zaimplementować operację dodawania z klasy `Num` dla tego nowego typu przechowującego kolory

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
    Czerowny + Zielony = Żółty
    Zielony + Czerowny = Żółty
    Zielony + Niebieski = Cyjan
    Niebieski + Zielony = Cyjan
    Czerwony + Niebieski = Fuksja
    Niebieski + Czerwony = Fuksja
    _ + _ = Czarny
```

![](https://i.chzbgr.com/maxW500/8548211712/hF0537D89/)

### Rozwiązanie
Mając typ danych `data Drzewo a = Nic | Węzeł a (Drzewo a) (Drzewo a)` zaimplementować dla niego instancję funktora

Właściwości funktorów:
```haskell
fmap id == id
fmap (f . g) == fmap f . fmap g
```

```haskell
data Drzewo a = Nic | Węzeł a (Drzewo a) (Drzewo a)

instance Functor Drzewo where
    fmap _ Nic                  = Nic
    fmap f (Węzeł x left right) =
        Węzeł (f x) (fmap f left) (fmap f right)
```

![](https://i.chzbgr.com/maxW500/8549208320/hD46DDB7B/)

### Rozwiązanie
Bazując na poprzedniej implementacji, zapisać dla typu `Drzewo` instancję aplikatora

Właściwości aplikatorów:
```haskell
pure id <*> v == v
pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
pure f <*> pure x == pure (f x)
u <*> pure y == pure ($ y) <*> u
```

```haskell
instance Applicative Drzewo where
    pure x = Węzeł x Nic Nic
    Nic <*> _ = Nic
    _ <*> Nic = Nic
    (Węzeł f f1 f2) <*> (Węzeł x x1 x2) =
        Węzeł (f x) (f1 <*> x1) (f2 <*> x2)
```

![](https://i.chzbgr.com/maxW500/8431718656/h1E8B69B5/)

### Rozwiązanie
```haskell
import Data.Char

data Ident a = Ident a deriving Show

main = do
    print (Ident 42)
    print $ upper `fmap` (Ident "abc")
    print $ (+) <$> (Ident 3) <*> (Ident 2)
    print $ return "def" >>= upperIdent

upper :: String -> String
upper = map toUpper

upperIdent :: String -> Ident String
upperIdent x = return (upper x)

instance Functor Ident where
    fmap f (Ident x) = Ident (f x)

instance Applicative Ident where
    pure x = Ident x
    (Ident f) <*> (Ident x) = Ident (f x)

instance Monad Ident where
    (Ident x) >>= f = f x
```

![](http://www.timemachinego.com/linkmachinego/wordpress/wp-content/uploads/2009/02/techcatpreview.jpg)
