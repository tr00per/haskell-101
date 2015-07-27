# Myśleć funkcyjnie #2

## Wzorcowanie i strażnicy

---
## System typów
![](http://imgs.xkcd.com/comics/types.png)

### Retrospekcja
```haskell

```

### Dane
```haskell
-- alias
type String = [Char]

-- "bardziej skryty" alias
newtype Money = Money Integer

-- własne dane
data Kolory = Czerwony | Zielony | Niebieski

data Drzewo a = Nic | Węzeł (Drzewo a) (Drzewo a)
```

### Klasy
```haskell

```

---
## Funktory
Na instancjach tej klasy można wywoływać "gołe" funkcje za pomocą `fmap`
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Dla list implementacja `fmap` to po prostu `map`. `fmap` jest ogólniejszą koncepcją.

### Maybe
```haskell
fmap (\x -> x*x) (Just 5)
fmap (\x -> x*x) Nothing
```

### Either
```haskell
fmap (\x -> x*x) (Right 5)
fmap (\x -> x*x) (Left "Failed")
```

---
## Aplikatory
Funkcje zamknięte w instancji tej klasy można zaaplikować na wartościach w niej zamkniętych
```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Dodatkowo jest też zdefiniowany alias: `(<$>) = fmap`.
```haskell
pure (+) <*> [1,2,3] <*> [1,2,3]
(+) <$> [1,2,3] <*> [1,2,3]
```

---
## Monady
### Maybe
### Writer
---

## Stan
### State
### I/O

---

## Embeded Domain-Specific Langage
