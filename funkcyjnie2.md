# Myśleć funkcyjnie #2

![](http://imgs.xkcd.com/comics/haskell.png)

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
Symbol `_` we wzorcu oznacza, że nie będziemy używać wartości znajdującej się na tej pozycji.

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

-- mogą być parametryzowane i rekurencyjne
data Drzewo a = Nic | Węzeł a (Drzewo a) (Drzewo a)
```

### Klasy
Klasy grupują operacje, które można wykonać na danych (mogą zawierać domyślną implementację):
```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
```

Klasy mogą zależeć od siebie:
```haskell
class (Eq a) => Ord a where
    (<) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    -- itd.
```

### GHCI i typy raz jeszcze
Aby wyświetlić typ wyrażenia w GHCI trzeba poprzedzić je komendą `:t` albo przestawić flagę `:set +t`.
Aby wyświetlić więcej informacji, np. zobaczyć zdefiniowane instancje klas, w GHCI trzeba poprzedzić je komendą `:i`.

Np. aby zobaczyć wszystkie typy danych, którym można sprawdzać równość, wystarczy wywołać:
```haskell
:i Eq
```

### Implementowanie klasy

Automatyczna implementacja niektórych klas:
```haskell
data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)
```

Ręczna implementacja klas:
```haskell
instance Num a => Num (Maybe a) where
    Nothing + _ = Nothing
    _ + Nothing = Nothing
    (Just x) + (Just y) = Just (x + y)
    -- uwaga: niepełna definicja, tylko jedna operacja jest zdefiniowana!

Just 1 + Just 2
-- Just 3
Nothing + Just 4
-- Nothing
```

Ważne: można nakładać ograniczenia kontekstu w definicji danych, ale w praktyce unika się tego i stosuje ograniczenia wyłącznie na funkcjach, które go potrzebują lub w definicjach klas.

### Standardowe klasy Haskela 98
Ilustracja prosto z [Haskell 98 Online Report](https://www.haskell.org/onlinereport/basic.html).

![](https://www.haskell.org/onlinereport/classes.gif)

Historycznie Funktory i Aplikatory, o których za chwilę, zostały wprowadzone do języka później, niż Monady, dlatego hierarchia wyglądała inaczej.

W GHC 7.10 (27 marca 2015) została wprowadzona propozycja [Applicative Monad](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal), która ma wejść do języka w raporcie Haskell 2014.

### Zadania
__Zadanie__: Stworzyć własny typ danych, który reprezentuje kolory

__Zadanie__: Zaimplementować operację dodawania z klasy `Num` dla tego nowego typu przechowującego kolory

---
## Funktory
Na instancjach tej klasy można wywoływać "gołe" funkcje za pomocą `fmap`
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Dla list implementacja `fmap` to po prostu `map`. `fmap` jest ogólniejszą koncepcją.

Implementacja funktorów powinna spełnić pewne właściwości, które jednak są wymagane jedynie przez konwencję
```haskell
fmap id == id
fmap (f . g) == fmap f . fmap g
```

### Maybe
```haskell
data Maybe a = Nothing | Just a

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

fmap (\x -> x*x) (Just 5)
fmap (\x -> x*x) Nothing
```

`Maybe` jest jednym z podstawowych narzędzi i standardowa biblioteka posiada kilka bardzo przydatnych funkcji pomocniczych, które siedzą w module `Data.Maybe`, np.:
```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
lookup :: Eq a => a -> [(a, b)] -> Maybe b
catMaybes :: [Maybe a] -> [a]
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
```

Przykład użycia
```haskell
newtype Ident = Ident Int deriving Eq
type Login = String
data Użytkownik = Niezarejestrowany | Znany Login deriving Show

znajdź :: Ident -> [(Ident, Login)] -> Użytkownik
znajdź ident = maybe Niezarejestrowany Znany . lookup ident

main = print $ znajdź (Ident 3) [(Ident 3,"Ala")]
```

### Either
"You `Either` have a `Right` answer or you're `Left` with an error."
```haskell
data Either a b = Left a | Right b

fmap (\x -> x*x) (Right 5)
fmap (\x -> x*x) (Left "Failed")
```

`Either` również posiada zestaw funkcji pomocniczych, analogicznie w `Data.Either`, np.:
```haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c
lefts :: [Either a b] -> [a]
rights :: [Either a b] -> [b]
partitionEithers :: [Either a b] -> ([a], [b])
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

Implementacja aplikatora zapewnia nam, że możemy komponować funkcje z danymi zamkniętymi w "pudełkach" bez wcześniejszego ich "odpakowywania". Wygode i precyzyjne rozwiązanie.

Oczywiście aplikatory również powinny posiadać pewne właściwości:
```haskell
pure id <*> v == v
pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
pure f <*> pure x == pure (f x)
u <*> pure y == pure ($ y) <*> u
```

Przykład z wcześniej, czyli dlaczego nie potrzebujemy domyślnej implementacji `Num (Maybe a)`:
```haskell
(+) <$> Just 1 <*> Just 2
(+) <$> Nothing <*> Just 4
```

### Zadania
__Zadanie__: Mając typ danych `data Drzewo a = Nic | Węzeł a (Drzewo a) (Drzewo a)` zaimplementować dla niego instancję funktora

__Zadanie__: Bazując na poprzedniej implementacji, zapisać dla typu `Drzewo` instancję aplikatora

---
## Monady
![](http://image.spreadshirtmedia.net/image-server/v1/products/115205650/views/1,width=350,height=350,appearanceId=5.png)

Aby coś było Monadą - wystarczy, że będzie miało zdefiniowane dwie operacje:
* `return :: a -> m a` operacja, która umieszcza wartość w pojemniku, działa jak `pure`
* `(>>=) :: m a -> (a -> m b) -> m b` operacja łącząca dwie monadyczne funkcje

Druga operacja nazywa się "bind" i przyrzymy jej się bliżej.
### (>>=)
Przypomnijmy operator łączenia `(.)`:
$$
f(g(x)) = (f \circ g)(x)
$$
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c

-- użycie
(f . g) x
```

Typ binda jest nieco inny:
```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b

-- użycie
g x >>= f
```

Niepodobny do niczego. Jest jednak jeszcze jeden, podobny operator:
```haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

-- użycie
(f <=< g) x
```

To już jest znacznie bardziej podobne do łączenia funkcji przez `(.)`. `(<=<)` jest operatorem łączącym funkcje działające na monadach (monadowe).

Teraz jeden dodatkowy krok:
```haskell
(=<<) :: Monad m => (a -> m b) -> m a -> m b

-- użycie
f =<< g x
```

Teraz już widać, co jest grane. `(>>=)` jest odwróconym operatorem `(=<<)` (jest też `(>=>)`).

`(>>=)` jest bardziej praktyczny w użyciu, niż `(<=<)`, więc w praktyce ten pierwszy jest użyty do implementacji drugiego.

### Żargon i nerdowanie
Operatory łączenia funkcji monadycznych są też nazywane operatorami Kleisli

![](https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/Heinrich-Kleisli-1987.jpeg/220px-Heinrich-Kleisli-1987.jpeg)

Heinrich Kleisli był szwajcarskim matematykiem, którego nazwisko nosi kilka tworów w teorii kategorii, np. kategoria Kleisli lub trójka Kleisli.

### Notacja `do`
Zwykłyt zapis
```haskell
fun x =
    zapytanie x >>= \wynik -> let przemienione = transformuj wynik in zapisz przemienione >>= \_ -> return przemienione
```

Trochę zmieniamy formatowanie
```haskell
fun x =
    zapytanie x                          >>= \wynik ->
    let przemienione = transformuj wynik
    in zapisz przemienione               >>= \_ ->
    return przemienione
```

Notacja `do`
```haskell
fun x = do
    wynik <- zapytanie x
    let przemienione = transformuj wynik
    zapisz przemienione
    return przemienione
```

### Maybe
Propagowanie błędu
```haskell
import Data.Char
upper = map toUpper
users = [(1,"tr00per"), (2,"morlas"), (3,"sindagma")]

lookup 1 users >>= \user -> return (upper user)
lookup 10 users >>= \user -> return (upper user)

getUppercaseUserName ident = do
    user <- lookup ident users
    return (upper xs)
```

Prostszy przykład
```haskell
half :: Integral a => a -> Maybe a
half x | even x    = Just (x `div` 2)
       | otherwise = Nothing
```

### Writer
Logowanie.

Wyrwałem fragment ze swojej gry tekstowej. Nie będę twierdził, że to najpiękniejszy kod na świecie, ale za to ładnie ilustruje przypadek użycia monady `Writer`. Przechowuję `[String]`, bo każdy ciąg w tablicy to jedna linia.
```haskell
battle :: Player -> Creature -> Writer [String] (Player, BattleResult)
battle player@(toCreature -> pc) enemy
    | health pc <= 0 && health enemy <= 0 = do
        tell ["You're both dead."]
        return (player, Draw)
    | health pc <= 0    = do
        tell ["You're dead."]
        return (player, CreatureWon)
    | health enemy <= 0 = do
        tell ["You won!"]
        return (player, PlayerWon)
    | otherwise         = do
        newPlayer <- enemy `attack` pc
        newEnemy  <- pc `attack` enemy
        if pc == newPlayer && enemy == newEnemy
            then do tell ["Your attacks have no effect!"]
                    return (player, NoEffect)
            else battle (Player newPlayer) newEnemy

attack :: Creature -> Creature -> Writer [String] Creature
attacker `attack` defender = do
    let damage = max 0 (power attacker - armor defender)
    tell [getName attacker ++ " deals " ++ show damage ++ " damage to " ++ getName defender]
    return (reduceHealth defender damage)
```

---
## Stan

### State
Przechowywanie stanu między akcjami.

Poniżej niedoskonały, ale działający kalkulator parsujący Polish Prefix Notation.
```haskell
import Control.Monad.State

main = do
    print $ calc "- 4 + 2 3"
    print $ calc "+ + + 1 1 1 1"
    print $ calc "- * / 15 - 7 + 1 1 3 + 2 + 1 1"

data PPN = Data Double | Op (Double -> Double -> Double)

calc :: String -> Double
calc input =
    let tokens = words input
        stack = createStack tokens
    in evalState calculate stack

calculate :: State [PPN] Double
calculate = do
    it <- pop
    case it of
        Data r -> return r
        Op op  -> do
            x <- calculate
            y <- calculate
            return (op x y)
    where
        pop :: State [PPN] PPN
        pop = do
            (it:rest) <- get
            put rest
            return it

createStack :: [String] -> [PPN]
createStack tokens = map parse tokens where
    parse "+" = Op (+)
    parse "-" = Op (-)
    parse "/" = Op (/)
    parse "*" = Op (*)
    parse  x  = Data (read x)
```

### I/O
Kilka prostych przykładów
```haskell
hello = putStrLn "Hello, world!"

answer = putStrLn $ show 42
answer' = print 42

copy fin fout = readFile fin >>= writeFile fout

readSomeFile = getLine >>= readFile >>= putStrLn
```

Jeszcze raz kawałek kodu wyciągnięty z mojej gry tekstowej.
```haskell
saveAdventure :: Player -> DungeonState -> IO GameStatus.
saveAdventure player dstate = bracket (openFile saveGameName WriteMode) hClose storeData
    where storeData handle = do playerWritten <- tryEither (hPrint handle player)
                                dstateWritten <- tryEither (hPrint handle dstate)
                                return $ statusChanged playerWritten dstateWritten (\_ _ -> GameSaved)

loadAdventure :: IO GameStatus
loadAdventure = bracket (openFile saveGameName ReadMode) hClose loadData
    where loadData handle = do player <- readEither `liftM` hGetLine handle
                               dstate <- readEither `liftM` hGetLine handle
                               return $ statusChanged player dstate (curry GameLoaded)

```

### Zadania
__Zadanie__: Stworzyć implementację trywialnej monady, która nic nie robi, a jedynie zamyka w sobie wartość

## Podsumowanie

![](http://adit.io/imgs/functors/recap.png)

Ilustrację pożyczyłem z bardzo kolorowego omówienia tematu na [http://adit.io/](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

---
## Embeded Domain-Specific Langage
Algorytmy + struktury danych = programy

Z EDSL mamy doczynienia wtedy, kiedy wykorzystując elementy języka tworzymy w nim zestaw narzędzi, którymi łatwiej jest operować w danej dziedzinie wiedzy. Idea jest taka, żeby analityk biznesowy mógł razem z programistą stworzyć wspólny język, który będzie zrozumiały dla obu stron. Kompilowane zasady biznesowe.

Jednym podejściem jest stworzenie lub dołączenie zewnętrznego języka skryptowego. 

Przykładem zewnętrznego języka domenowego, o którym prawdopodobnie wszyscy słyszeliście, jest **SQL**. Jest to język domenowy do manipulacji obszernymi zbiorami danych z bazie relacynej.

Można tez pod tę definicję podciągnąć języki skryptowe używane w grach do programowania zachowań przeciwników (np. wybór najlepszej ścieżki; sam algorytm jej wyznaczania będzie zaimplementowany w głównym języku) lub modelowania zachowań środowiska (np.,, kiedy ma zacząć padać deszcz). **Lua** jest przykładem języka, który został zastosowany w wielu grach własnie do tego celu (Wikipedia wymienia 151 gier, które go wykorzystują, m. in.: Angry Birds, Baldur's Gate, Freeciv, Saints Row 2/3/4, Warhammer 40k: Dawn of War 1/2).

Jeśli język oferuje wystarczającą ekspresywność, można ominąć wprowadzenie zwenętrznego języka (i pisanie parsera albo wciąganie frameworku, jak [xText](https://eclipse.org/Xtext/) dla Javy).
