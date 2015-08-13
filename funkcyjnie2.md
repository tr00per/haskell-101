# Myśleć funkcyjnie #2

## Wzorcowanie i strażnicy
Ignorując na chwilę, że mapowanie jest tak naprawdę reprezentowane prez złożenie, funkcję `map` można zapisać w taki sposób:
```haskell
map f xs = if null xs
           then f (head xs) : map f (tail xs)
           else map f (tail xs)
```

Istnieje przejrzystszy sposób wyrażenia jej:
```haskell
map f []     = []
map f (x:xs) = f x : map f xs
```

A nawet jeszcze lepiej:
```haskell
map _ []     = []
map f (x:xs) = f x : map f xs
```

Mechanizm wzorcowania (pattern matching) znajduje zastosowanie w wielu miejscach w Haskellu. Zamiast bezpośrendio w nagłówku funkcji można go tez użyć wewnątrz:
```haskell
map f xs = case xs of
               []   -> []
               x:xs -> f x : map f xs
```

Mechanizmem, który często towarzyszy wzorcowaniu, są strażnicy (guards):
```haskell
legal 0             = False
legal x | x < -5    = False
        | x > 0     = True
        | otherwise = True
```

Również i w tym wypadku da się zastosować ten mechanizm wewnątrz ciała funkcji:
```haskell
legal x = case x of
              0             -> False
              x | x < -5    -> False
                | x > 0     -> True
                | otherwise -> True
```

---
## System typów
![](http://imgs.xkcd.com/comics/types.png)

### Retrospekcja
```haskell
() :: ()
[1,2,3,4,5] :: Num t => [t]
"Wąchock & 漢字" :: [Char]
(10, "Karmel") :: Num t => (t, [Char])
(True, -1)     :: Num t => (Bool, t)
[(1,"San Francisco"), (2, "New York")] :: Num t => [(t, [Char])]

(:) :: a -> [a] -> [a]
[]  :: [a]

head :: [a] -> a
tail :: [a] -> [a]
take :: Int -> [a] -> [a]

filter :: (a -> Bool) -> [a] -> [a]
map    :: (a -> b) -> [a] -> [b]
foldr  :: (a -> b -> b) -> b -> [a] -> b
foldl  :: (a -> b -> a) -> a -> [b] -> a

odd  :: Integral a => a -> Bool
even :: Integral a => a -> Bool
div  :: Integral a => a -> a -> a
mod  :: Integral a => a -> a -> a
(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool
(==) :: Eq a => a -> a -> Bool
(/=) :: Eq a => a -> a -> Bool
sqrt :: Floating a => a -> a

print :: Show a => a -> IO ()

sqr  :: Num a => a -> a
(>:) :: [a] -> a -> [a]
lessthan8 :: (Num a, Ord a) => a -> Bool
(5+)      :: Num a => a -> a
wybierz       :: Num a => (a -> a -> Bool) -> a -> a -> a
polityka1     :: Ord a => a -> a -> Bool
bank1_wybierz :: (Num a, Ord a) => a -> a -> a
```

### Dane
Aliasy

```haskell
-- alias
type String = [Char]

-- "bardziej skryty" alias
newtype Money = Money Integer
```

"Dane"
```
-- typ parametryzowany
data Maybe a = Nothing | Just a

-- własne dane
data Kolory = Czerwony | Zielony | Niebieski

data Drzewo a = Nic | Węzeł (Drzewo a) (Drzewo a)
```

### Klasy
Klasy grupują operacje, które można wykonać na danych:
```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=)
```

Automatyczna implementacja niektórych klas:
```haskell
data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)
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
(+) `fmap` [1,2,3] <*> [1,2,3]
(+) <$> [1,2,3] <*> [1,2,3]
```

---
## Monady
Aby coś było Monadą - wystarczy, że będzie miało zdefiniowane dwie operacje:
* `return :: a -> m a` operacja, która umieszcza wartość w pojemniku, działa jak `pure`
* `(>>=) :: m a -> (a -> m b) -> m b` operacja łącząca dwie monadyczne funkcje

### Maybe
```haskell

```

### Writer
```haskell

```

---
## Stan
Innym przydatnym zastosowaniem monad jest ukrywanie machinerii zarządzania stanem.

### State
```haskell

```

### I/O
```haskell

```

---
## Embeded Domain-Specific Langage
Algorytmy + struktury danych = programy

Z EDSL mamy doczynienia wtedy, kiedy wykorzystując elementy języka tworzymy w nim zestaw narządzi, którymi łatwiej jest operować w danej dziedzinie wiedzy.