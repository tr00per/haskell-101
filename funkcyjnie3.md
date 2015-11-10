# Myśleć funkcynie #3

## Funktory i Aplikatory

![](http://adit.io/imgs/functors/fmap.png)

![](http://adit.io/imgs/functors/applicative.png)

Ilustracje pożyczyłem z bardzo kolorowego omówienia tematu na [http://adit.io/](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

## Monady
![](http://image.spreadshirtmedia.net/image-server/v1/products/115205650/views/1,width=350,height=350,appearanceId=5.png)

Aby coś było Monadą wystarczy, że będzie miało zdefiniowane dwie operacje:
* `return :: Monad m => a -> m a` operacja, która umieszcza wartość w pojemniku, synonim `pure`
* `(>>=) :: Monad m => m a -> (a -> m b) -> m b` operacja łącząca dwie monadyczne funkcje

Druga operacja nazywa się "bind" i za moment przyjrzymy jej się bliżej.

Klasa monady udostępnia jeszcze dwie operacje:
* `(>>) :: Monad m => m a -> m b -> m b` tylko zaznacza następstwo akcji, nie przekazuje rezultatu pierwszej akcji do drugiej
* `fail :: Monad m => String -> m a` służy przerwaniu akcji z komunikatem błędu. Ostrożnie z używaniem!

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

To już jest znacznie bardziej podobne do łączenia funkcji przez `(.)`. `(<=<)` jest operatorem łączącym funkcje działające na monadach.

Teraz jeden dodatkowy krok:
```haskell
(=<<) :: Monad m => (a -> m b) -> m a -> m b

-- użycie
f =<< g x
```

`(>>=)` jest odwróconym operatorem `(=<<)` (jest też `(>=>)`). Teraz już widać, co jest grane!

Możemy też napisać:
```haskell
bindrl f = join . fmap f
-- jak (=<<)

bindlr ma f = join . fmap f $ ma
-- jak (>>=)
```

Zachodzi również poniższa zależność z funktorem i aplikatorem:
```haskell
fmap f xs ==  xs >>= pure . f
```

### Żargon i nerdowanie
Operatory łączenia funkcji monadycznych są też nazywane operatorami Kleisli

![](https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/Heinrich-Kleisli-1987.jpeg/220px-Heinrich-Kleisli-1987.jpeg)

Heinrich Kleisli był szwajcarskim matematykiem, którego nazwisko nosi kilka tworów w teorii kategorii, np. kategoria Kleisli lub trójka Kleisli.

Monoid to inaczej półgrupa z jedynką. Półgrupa z jedynką to zbiór wartości, wewnętrzna operacja zdefiniowana na nim i element neutralny ("jedynka"), np. $$(\mathbb{R}, *, 1)$$, $$(\mathbb{R}, +, 0)$$.
Zbiór funkcji `a -> m b`, operator Kleisli `>=>` (albo `<=<`) i funkcja `return` (albo `pure`) tworzą półgrupę z jedynką.

![](http://vignette2.wikia.nocookie.net/gameofthrones/images/7/7f/Daenerys_and_dragons_2x10.png/revision/latest?cb=20120604062453)

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

### Zastrzeżenie
Poniżej znajduje się lista monad, która jednak nie aspiruje do bycia kompletną i definitywną. Pomijamy chociażby monadę ST, która daje nam m. in. jednowątkowe zmienne.

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

Nie chodzi jednak o łączenie w krotki, do tego służą funkcje z rodziny `zip` albo aplikator `ZipList`:
```haskell
zip [1,2,3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]

zip3 [1,2,3] "abc" [10..]
-- [(1,'a',10),(2,'b',11),(3,'c',12)]

zipWith (*) [3,4,5] [4,2,1]
-- [12,8,5]

zipWith3 (\x y z -> x+y*z) [1..] [2..] [5,4,3]
-- [11,14,15]

import Control.Applicative
(\x y -> x*y) <$> ZipList [1..3] <*> ZipList [1..]
-- ZipList {getZipList = [1,4,9]}
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
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) --where ...
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
### Reader

Konfiguracja.

Uproszczona definicja
```haskell
newtype Reader r a = Writer { runReader :: r -> a }

instance (Monoid w) => Monad (Writer w) --where ...
```

Analogicznie operacje są skupione w klasie `MonadReader`, z czego najważniejszą jest `ask`. Reader dostarcza nam niemodyfikowalną strukturę, którą możemy przywołać w naszym kodzie, by następnie wyłuskać z niej potrzebną wartość.

## Stan

### State
Przechowywanie stanu między akcjami.

Uproszczona definicja
```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

Operacja zawarte w `MonadState`, a najważniejsze z nich to `set` i `get`. Dla tej monady mamy również zestaw funkcji "uruchamiających":

```haskell
runState :: State s a -> s -> (a, s)

evalState :: State s a -> s -> a
-- fst . runState

execState :: State s a -> s -> s
-- snd . runState
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

### IO
Dowolne efekty uboczne. Komunikacja ze światem zewnętrznym, współbieżność, wyświetlanie, manipulacja plikami, kontrolowanie Matriksa.

Imaginacja definicji
```haskell
newtype IO realWorld a = IO { runIO :: realWorld -> (a, realWorld) }
```

W rzeczywistości nie mamy dostępu do obiektu "prawdziwego świata", zarządza nim środowisko uruchomieniowe.

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

### Co z tym `fail`em?
Dlaczego trzeba uważać z funkcją `fail`? Ponieważ w monadzie `IO` rzuci nam wyjątkiem, który nieprzechwycony położy całą aplikację. Musimy o tym pamiętać, jeśli będziemy wchodzić w interakcje z monadą, która nie ma swojej reprezentacji błędu.

### Zadania
__Zadanie__: Stworzyć implementację trywialnej monady, która nic nie robi, a jedynie zamyka w sobie wartość

## Podsumowanie

![](http://adit.io/imgs/functors/recap.png)

Ilustrację znowu pożyczyłem z [http://adit.io/](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

---

## Transformatory Monad

Transformatory pozwalają składać ze sobą monady.

Prawdziwa definicja wcześniejej wymienionych monad:
```haskell
type Writer w = WriterT w Identity

type Reader r = ReaderT r Identity

type State s = StateT s Identity
```

Zamiast monady identyczności możemy ułożyć sobie własny stos efektów, które będą mieć znaczenie dla naszej aplikacji.

![](monadowa_matrioszka.jpg)

Zawarłem na tym obrazku dwa niedopowiedzenia, ale i tak jest to ładna ilustracja (zdjęcie z Internetów, podpisy moje).

(1) Transformatory mogą być w dowolnej kolejności i jeszcze do tego się powtarzać, natomiast (2) nasza generyczna wartość `a` jest z punktu widzenia kodu parametrem zewnętrznego transformatora.

`IO` nie ma swojego transformatora i jeśli chcemy użyć komunikacji z zewnętrznym światem, to musi się ona znajdować u podstawy naszego stosu efektów.

### Przykład użycia stosu transformatorów
Wzorowane na przykłądach z [Real World Haskell](http://book.realworldhaskell.org/read/monad-transformers.html).

```haskell
type AppLog = [String]
type AppState = [Integer]
data AppConfig = AppConfig {
    maxValue :: Integer
} deriving Show


appMain :: WriterT AppLog (ReaderT AppConfig (StateT AppState IO)) ()

run :: WriterT AppLog (ReaderT AppConfig (StateT AppState IO)) () -> AppConfig -> AppState -> IO (((), AppLog), AppState)
```

Nie da się tego normalnie używać... Ale od czego są aliasy!
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Application a = Application {
    runApp :: WriterT AppLog (ReaderT AppConfig (StateT AppState IO)) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadWriter AppLog, MonadReader AppConfig, MonadState AppState)

newtype AppResult a = AppResult { getResult :: IO ((a, AppLog), AppState) }

run :: Application () -> AppConfig -> AppState -> AppResult ()
run app config initState = AppResult (runStateT (runReaderT (runWriterT (runApp app)) config) initState)

appMain :: Application ()
appMain = ...
```

To teraz jeszcze krótkie ciało programu, żeby zaprezentować, że mamy dostęp do wszystkich potrzebnych rzeczy
```haskell
appMain :: Application ()
appMain = do
    putStrLn' "Zaczynam!"
    tell ["Zaczęło się..."]
    oldValue <- get
    putStrLn' $ "Początkowa wartość stanu: " ++ show oldValue
    limit <- maxValue <$> ask
    tell ["Odczytałem limit " ++ show limit]
    putStrLn' $ "Limit to " ++ show limit
    put [limit * 10]
    newValue <- get
    putStrLn' $ "Wartość stanu: " ++ show newValue
    tell ["Kończymy..."]
    putStrLn' "Skończyłem!"
    where
        putStrLn' = liftIO . putStrLn
```

![](https://refugeestrength.files.wordpress.com/2013/12/shock25.jpg)

I jeszcze main, żeby to wszystko ze sobą połączyć:
```haskell
main :: IO ()
main = do
    putStrLn "Hi"
    args <- getArgs
    let config = AppConfig $ case args of
                     []    -> 10
                     (x:_) -> read x
    print config
    result <- getResult $ run appMain config 300
    let ((_, logs), finalState) = result
    putStrLn $ "\nLogi: " ++ unlines logs
    putStrLn $ "\nOstatni stan: " ++ show finalState
    putStrLn "Bye"
```

Efekt działania programu:
```bash
$ ./monad_transformers 18
Hi
AppConfig {maxValue = 18}
Zaczynam!
Początkowa wartość stanu: 300
Limit to 18
Wartość stanu: 180
Skończyłem!

Logi: Zaczęło się...
Odczytałem limit 18
Kończymy...


Ostatni stan: 180
Bye
```

bez argumentu też działa:
```bash
$ ./monad_transformers
Hi
AppConfig {maxValue = 10}
Zaczynam!
...
```

Komplet importów dla naszej aplikacji wygląda tak:
```haskell

import System.Environment (getArgs)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
```

### Przypadek szczególny

Jeśli przyjdzie nam nałożyć na siebie dwie monady tego samego typu, to zaczyna się robić nieco ślisko. System typów będzie nas trzymał w pionie, ale troską trzeba otoczyć zdrowie psychiczne.

Przykład prostego homozłożenia:
```haskell
type DoubleState = StateT Int (State String)
```

Teraz żeby dobrać się do zewnętrzenego stanu wystarczy wywołać `get` albo `set`. Jak natomiast dobrać się do wewnątrz?

```haskell
innerPut :: String -> DoubleState ()
innerPut = lift . put
```

Jednak na tym zabawa się nie kończy, bo jeśli zechcemy dołączyć więcej informacji i wciąż mieć dostęp do głębszego stanu, to znów musimy zrobić to jawnie.


```haskell
type BigStack = ReaderT Bool DoubleState

bigPut :: String -> BigStack ()
bigPut = lift . lift . put
```

![](http://queenofthenerds.net/wp-content/uploads/2013/12/safe_image.php_.jpeg)

### Zadania
__Zadanie__: Stworzyć implementację transformatora `MaybeT`, który dodaje do naszego stosu możliwość porażki.

Szablon na dobry początek:
```haskell
newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

fmapMT :: (Functor m) => (a -> b) -> MaybeT m a -> MaybeT m b

pureMT :: (Applicative m) => a -> MaybeT m a

apMT :: (Applicative m) => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b

failMT :: (Monad m) => t -> MaybeT m a
```

---

![](http://vignette2.wikia.nocookie.net/looneytunes/images/e/e1/All.jpg/revision/latest?cb=20150313020828)

![](https://cdn2.hubspot.net/hub/300222/file-666003009-jpg/images/better-breathing-track-and-field-powerlung.jpg)