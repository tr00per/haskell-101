# Myśleć funkcynie #3

## Funktory i Aplikatory

![](http://adit.io/imgs/functors/fmap.png)

![](http://adit.io/imgs/functors/applicative.png)

Ilustracje pożyczyłem z bardzo kolorowego omówienia tematu na [http://adit.io/](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

## Monady
![](http://image.spreadshirtmedia.net/image-server/v1/products/115205650/views/1,width=350,height=350,appearanceId=5.png)

Aby coś było Monadą - wystarczy, że będzie miało zdefiniowane dwie operacje:
* `return :: a -> m a` operacja, która umieszcza wartość w pojemniku, synonim `pure`
* `(>>=) :: m a -> (a -> m b) -> m b` operacja łącząca dwie monadyczne funkcje

Druga operacja nazywa się "bind" i przyjrzymy jej się bliżej.
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

Możemy też napisać:
```haskell
bindrl f = join . fmap f
-- jak (=<<)

bindlr ma f = join . fmap f $ ma
-- jak (>>=)
```

### Żargon i nerdowanie
Operatory łączenia funkcji monadycznych są też nazywane operatorami Kleisli

![](https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/Heinrich-Kleisli-1987.jpeg/220px-Heinrich-Kleisli-1987.jpeg)

Heinrich Kleisli był szwajcarskim matematykiem, którego nazwisko nosi kilka tworów w teorii kategorii, np. kategoria Kleisli lub trójka Kleisli.

Monoid to inaczej półgrupa z jedynką. Półgrupa z jedynką to zbiór wartości, wewnętrzna operacja zdefiniowana na nim i element neutralny ("jedynka"), np. $$(\mathbb{R}, *, 1)$$, $$(\mathbb{R}, +, 0)$$.
Zbiór funkcji `a -> m b`, operator Kleisli `>=>` (albo `<=<`) i funkcja `return` (albo `pure`) tworzą półgrupę z jedynką.

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

Dlatego monady nazywa się też "programowalnym średnikiem".

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

### List
Lista również jest monadą, a operacja na niej zdefiniowana dotyczy łączenia ze sobą dwóch list.

Nie chodzi jednak o łączenie w krotki, do tego służą funckje z rodziny `zip`:
```haskell
zip [1,2,3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]

zip3 [1,2,3] "abc" [10..]
-- [(1,'a',10),(2,'b',11),(3,'c',12)]

zipWith (*) [3,4,5] [4,2,1]
-- [12,8,5]

zipWith3 (\x y z -> x+y*z) [1..] [2..] [5,4,3]
-- [11,14,15]
```

Implementacja `>>=` zapewnia nam wywołanie przekazanej funkcji dla każdego elementu wejściowej listy
```haskell
Prelude> :t ("abc" >>=)
-- ("abc" >>=) :: (Char -> [b]) -> [b]

import Data.Char
"abc" >>= \x -> [toUpper x]
-- "ABC"

[3,4,5] >>= \x -> [4,5,6] >>= \y -> [x * y]
-- [12,15,18,16,20,24,20,25,30]
```

O ile taka forma zapisu jest mało intuicyjna na pierwszy rzut oka, to istnieje kolejny cukier składniowy, przeznaczony dla list, czyli wyrażenie listowe (_list comprehension_)
```haksell
[ toUpper x | x <- "abc" ]
-- "ABC"

[ x * y | x <- [3,4,5], y <- [4,5,6] ]
-- [12,15,18,16,20,24,20,25,30]

[ x * y | x <- [3,4,5], y <- [4,5,6], x > y ]
-- [20]
```

Wszystko to prowadzi nas do flagowego przykładu na leniwe obliczanie, czyli ciąg Fibonacciego!
```haskell
fib = 0:1:[ x + y | (x,y) <- zip fib (tail fib) ]

take 20 fib
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
```

Oto nieskończona lista wyrazów ciągu Fibonacciego, która oblicza samą siebie w miarę jak się ją oblicza ;)

### Writer
Logowanie.

Uproszczona definicja
```haskell
data Writer w a = Writer { runWriter :: w -> (a, w) }
```

Dostępne operacje są skupione w klasie `MonadWriter`, z czego najważniejszą jest `tell`. 

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

Uproszczona definicja
```haskell
data Writer w a = Writer { runWriter :: w -> (a, w) }
```

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

Ilustrację znowu pożyczyłem z [http://adit.io/](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

---
## Embeded Domain-Specific Langage
Algorytmy + struktury danych = programy

Z EDSL mamy doczynienia wtedy, kiedy wykorzystując elementy języka tworzymy w nim zestaw narzędzi, którymi łatwiej jest operować w danej dziedzinie wiedzy. Idea jest taka, żeby analityk biznesowy mógł razem z programistą stworzyć wspólny język, który będzie zrozumiały dla obu stron. Kompilowane zasady biznesowe.

Jednym podejściem jest stworzenie lub dołączenie zewnętrznego języka skryptowego. 

Przykładem zewnętrznego języka domenowego, o którym prawdopodobnie wszyscy słyszeliście, jest **SQL**. Jest to język domenowy do manipulacji obszernymi zbiorami danych z bazie relacynej.

Można tez pod tę definicję podciągnąć języki skryptowe używane w grach do programowania zachowań przeciwników (np. wybór najlepszej ścieżki; sam algorytm jej wyznaczania będzie zaimplementowany w głównym języku) lub modelowania zachowań środowiska (np.,, kiedy ma zacząć padać deszcz). **Lua** jest przykładem języka, który został zastosowany w wielu grach własnie do tego celu (Wikipedia wymienia 151 gier, które go wykorzystują, m. in.: Angry Birds, Baldur's Gate, Freeciv, Saints Row 2/3/4, Warhammer 40k: Dawn of War 1/2).

Jeśli język oferuje wystarczającą ekspresywność, można ominąć wprowadzenie zwenętrznego języka (i pisanie parsera albo wciąganie frameworku, jak [xText](https://eclipse.org/Xtext/) dla Javy).
