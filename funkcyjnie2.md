# Myśleć funkcyjnie #2
## System typów
![](http://imgs.xkcd.com/comics/types.png)

### Retrospekcja
### Dane
### Klasy

---
## Funktory
Na instancjach tej klasy można wywoływać "gołe" funkcje za pomocą `fmap`
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Dla list implementacja `fmap` to po prostu `map`. `fmap` jest ogólniejszą koncepcją.
```haskell
```

---
## Aplikatory
-Funkcje zamknięte w instancji tej klasy można zaaplikować na wartościach w niej zamkniętych
```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```
Dodatkowo jest też zdefiniowany alias: `(<$>) = fmap`.

```haskell
```

---
## Monady
### Maybe
### Writer
---

## Stan
### Monada stanu
### I/O

---

## Embeded Domain-Specific Langage
