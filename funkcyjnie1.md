# Myśleć funkcyjnie #1
Cała przyjemność polega na tym, żeby małą ilością kodu wyrazić możliwie dużo. Na usuwaniu szumu z kodu.

$$
SNR = P_{signal}/P_{noise}
$$

---
## Listy i krotki
W programowaniu funkcyjnym powszechnie używa się list zamiast tablic
```haskell
-- Lista liczb
[1,2,3,4,5]

-- "Odcukrzona" wersja
1:2:3:4:5:[]

-- Lista znaków
['H','a','s','k','e','l','l']
"Curry" -- w Haskellu typ String jest aliasem dla tablicy znaków
"Wąchock & 漢字" -- Unicode!

-- Generatory
[1..10]
[1,3..10]
```
Wszystkie elementy w liście muszą być tego samego typu. Aby móc mieszać typy można użyć krotki
```haskell
(10, "Karmel")
(True, -1)
(40, 255, 0)

-- lista krotek - lista asocjacyjna
[(1,"San Francisco"), (2, "New York")]
```
**Listy** mogą przechowywać zero albo więcej **elementów jednego typu**, natomiast *__krotki__* mogą *__mieszać typy elementów__*, jednak raz zdefiniowanych kolejności i rozmiaru nie da sie zmienić

Przy okazji: unit, czyli bezwartościowa wartość
```haskell
()
```

Listy dzielą się na "głowę" i "ogon":
```haskell
head [1..5]
-- 1
tail [1..5]
-- [2,3,4,5]
```

Nieskończone listy
```haskell
[1..]
```

![To infinity and beyond!](http://img.interia.pl/rozrywka/nimg/2/7/roz4286600.jpg)

### Leniwe obliczanie
Spróbujcie tego
```haskell
take 10 [1..]
head [1..]
```

Interpreter się nie zawiesza, ponieważ generowane jest tylko tyle, ile potrzeba do zaspokojenia żądania.

Przykład, kiedy przyda się leniwa ewaluacja - pisząc funkcję, która generuje ciąg możliwych rozwiązań problemu (np. wypełnione plansze do sudoku) nie musimy przejmować się, żeby w tej samej funkcji sprawdzać poprawność wypełnień. Możemy zrobić to w osobnej funkcji, a na koniec stwierdzić tylko "chcę pierwsze, co wyjdzie".
```haskell
head $ filter correct_solution $ generate_values data
-- albo: head (filter correct_solution (generate_values data))
```

Inny przykład - separacja IO od obliczeń. Zamiast samemu troszczyć się o to, by jak najszybciej wyświetlać wygenerowane modele, tworzymy funkcję, które je konstruuje, a potem mówimy "wyświetl wszystko". Leniwa ewaluacja zapewni nam, że modele będą tworzone dopiero wtedy, kiedy będą faktycznie potrzebne (nie będzie wielkiej pauzy na początku), a każdy będzie wyświetlony jak tylko będzie gotowy.
```haskell
display $ prepare_geometry source
```

---
## Filtrowanie
```haskell
odd 1
even 1

filter odd [1..10]
```
`filter` jest funkcją wyższego rzędu, przyjmuje jako jeden ze swoich argumentów inną funkcję!

Dla porównania w C++:
```cpp
#include <iostream>
#include <list>

std::list<int> filter(bool (*predicate)(int), const std::list<int>& input_list)
{
    std::list<int> output_list;
    for (const int entry : input_list)
    {
        if (predicate(entry))
        {
            output_list.push_back(entry);
        }
    }
    return std::move(output_list);
}

int main()
{
    std::list<int> my_data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto odd = [](int x) -> bool {return x % 2 != 0;};
    std::list<int> filtered_data = filter(odd, my_data);
    for (auto a : filtered_data)
    {
        std::cout << a << ", ";
    }
    std::cout << std::endl;
    
    return 0;
}
```

![](http://i1.kym-cdn.com/photos/images/original/000/000/681/what-you-did-there-i-see-it.thumbnail.jpg)

Ok, to nie było do końca uczciwe, pełny program w Haskellu wyglądałby tak:
```haskell
main = print (filter odd [1..10])
```

![](http://i3.kym-cdn.com/entries/icons/original/000/001/987/fyeah.jpg)

To jeszcze w Scali:
```scala
object Main extends App {
	def odd = (x : Int) => x % 2 != 0
	val filtered = (1 to 10) filter odd
	System.out.println(filtered)
}
```

### Definiowanie własnej funkcji w GHCI
```haskell
let myfunc x = x * x
```

Aby `filter` zaakceptował naszą funkcję, musi ona przyjmować jeden argument, którego typ musi zgadzać się z typem przechowywanym w liście. Wartością zwracaną musi być typu `Bool`.

### GHCI i typy
Aby wyświetlić typ wyrażenia w GHCI trzeba poprzedzić je komendą `:t` albo przestawić flagę `:set +t`.

### Zadania
__Zadanie__: Zdefiniować własną funckję `odd` albo `even` i użyć jej do przefiltrowania listy.

__Zadanie__: Napisać funckję, która posłuży do odfiltrowania liczb, które są podzielne przez 4, ale nie przez 3

Przydatne funkcje: `div`, `mod`, `&&`, `||`, `==`, `/=`. 

---
## Mapowanie
W językach funkcyjnych nie ma pętli.

A przed chwilą wykonaliśmy serię operacji na liście wartości i wcale jej nam nie brakowało!

![Shock!](http://www.pagefield.co.uk/wp-content/uploads/2013/06/shock.jpg)

Filtrowanie jest bardzo popularną operacją na liście elementów. Równie powszechną, jeśli nie wszechobecną, operacją jest transformacja wartości w liście na nowe wartości. W Haskellu taka operacja wytwarza nową listę.
```haskell
map odd [1..5]
map sqrt [1..5]
map toUpper "Amsterdam" -- wymaga modułu Data.Char
```

C++ (samo gęste)
```cpp
std::list<double> somelist = {1, 2, 3, 4, 5}, outlist(5);
std::transform(somelist.begin(), somelist.end(), outlist.begin(), (double(*)(double))std::sqrt);
```

Scala
```scala
(1.0 to 5.0 by 1.0) map (Math.sqrt)
```

### W głąb mapy
Jak mogłaby wyglądać implementacja funkcji `map`?
```haskell
map f xs = if null xs
           then f (head xs) : map f (tail xs)
           else map f (tail xs)
```

Mamy przykład "rekurencji ogonowej" (tail recursion).

---
## Łączenie
Pamiętacie z matematyki łączenie funkcji?

$$
f(g(x)) = (f \circ g)(x)
$$

W Haskellu też można łączyć funkcje (duh...), a nawet jest udostępniony do tego specjalny operator!
```haskell
h x = f (g x)
h' x = (f . g) x
```

Co może być na początku nieintuicyjne, funkcje połączone za pomocą operatora `(.)` są aplikowane od prawej do lewej - tak jak w matematycznym odpowiedniku.

Nie wszystkie funkcje da się ze sobą łączyć - na styku typy wartości zwracanej i argumentu muszą się zgadzać.

### Łączenie i mapowanie
```haskell
let sqr x = x * x
let lessthan8 x = x < 8

:t sqr
-- sqr :: Num a => a -> a
:t lessthan8
-- lessthan8 :: (Num a, Ord a) => a -> Bool

:t (lessthan8 . sqr)
-- (lessthan8 . sqr) :: (Num a, Ord a) => a -> Bool

let pośrednia = map sqr [1..5]
-- [1,4,9,16,25]
map lessthan8 pośrednia
-- [True,True,False,False,False]

map (lessthan8 . sqr) [1..5]
-- [True,True,False,False,False]
```

---
## Lambda
![](http://vignette1.wikia.nocookie.net/half-life/images/c/c9/Half-Life_Wiki_Logo.png/revision/20130801093040?path-prefix=en)

Jeśli nie żyliście pod kamieniem przez ostatnie kilka lat, to słyszeliście o funkcjach lambda.

To taka funkcja, którą definiujemy "na kolanie", bo jest za krótka, żeby zaprzątać nią szerszą przestrzeń nazw.
```haskell
myfilter xs = filter (\x -> x % 4 == 0 && x % 3 \= 3) xs

squares xs = map (\x -> x * x) xs
```

Haskell udostępnie też dwa inne mechanizmy do definiowania lokalnych nazwanych funkcji, więc w produkcyjnych warunkach możecie zobaczyć ich znacznie mniej, niż w podręcznikach czy tutaj.

C++
```cpp
auto sqr = [](int x) -> int {return x * x;};
auto sqr_tpl = [](auto x) -> decltype(x) {return x * x;};
```

Scala
```scala
def sqr = (x : Int) => x * x
def sqr_tpl[A](x: A)(implicit numeric: Numeric[A]): A = numeric.times(x, x)
```

### Żargon i nerdowanie
Konwersja Eta (η) - proces dodawania albo ujmowania abstrakcji od funkcji.
* Eta-redukcja: `\x -> abs x` zamieniamy w `abs`
* Eta-abstrakcja: `abs` zamieniamy w `\x -> abs x`
 
Kolejne aplikowanie η-redukcji jest trzonem stylu programowania "bezpunktowego" (_pointfree_, dla złośliwych _pointless_).
```haskell
add''' = (+)
(>:) = flip (:)

h x = f (g x)
h' x = (f . g) x
h'' = f . g
```

Styl ten jest czasm pomocny - stosowaliśmy go tutaj - jednak łatwo doprowadzić do poziomu abstrakcji, który będzie nieczytelny nawet dla autora. Dlatego zalecany jest umiar.

Ciekawostki
```haskell
map f . map g == map (f . g)
map f . filter (p . f) == filter p . map f
```

---
## Currying
![](https://wiki.haskell.org/wikiupload/8/86/HaskellBCurry.jpg)

Currying to "wielka rzecz" w językach takich jak Scala, ponieważ odnosi się ją do jedynej słusznej konwencji wywołania funkcji w Javie.

```scala
def modN_uncurried(n: Int, x: Int) = x % n == 0
def modN_curried(n: Int)(x: Int) = x % n == 0
```

Chodzi o to, że funckję, która przyjmuje ustaloną liczbę elementów, można zamienić na serię funkcji, które przyjmują tylko jeden argument. Pojęcie to łączy się z _częściową aplikacją_.

W Haskellu częściowa aplikacja jest powszechnie używana.
```haskell
add x y = x + y
add5 = add 5

add' = (+)
add5' = (5+)

map f xs = foldr ((:) . f) [] xs
map' f = foldr ((:) . f) []
```

Można pomyśleć, że wszystkie funkcje w Haskellu tak naprawdę pod spodem składają się z serrii jednoargumentowych funkcji
```haskell
add x y = x + y
add' x = \y -> x + y
add'' = \x -> \y -> x + y
```

### Żargon i nerdowanie
Curring tak naprawdę został stworzony przez rosyjskiego matematyka i twórcę rachunku kombinatorów: Mosesa Schönfinkela. Haskell Curry, który z kolei był amerykaninem, rozwinął koncepcję Schönfinkela.

![](https://upload.wikimedia.org/wikipedia/commons/9/97/Schonfinkel.gif)

### Bliżej rzeczywistości
```haskell
-- wybierz :: Num a => (a -> a -> Bool) -> a -> a -> a
wybierz polityka dostępne rządane = if polityka dostępne rządane
                                    then dostępne - rządane
                                    else dostępne

-- polityka1 :: Ord a => a -> a -> Bool
polityka1 ma chce = ma >= chce

-- bank1_wybierz :: (Num a, Ord a) => a -> a -> a
bank1_wybierz = wybierz polityka1
```

---
## Składanie
Jest jeszcze jedna operacja wyższego rzędu, którą warto przyswoić, ponieważ znajduje się na jeszcze wyższym poziomie abstrakcji, niż `filter` czy `map`.

```haskell
map f xs = foldr ((:) . f) [] xs

filter p xs = foldr (pred p) [] xs
    where pred f x acc = if f x then x:acc else acc
```

Składanie w Haskellu, występuje w dwóch odmianach
* prawostronne

```haskell
foldr (+) 0 [1..10]
-- 0 + (1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + 10)))))))))

foldr (:) [] [1..10]
-- 1:(2:(3:(4:(5:(6:(7:(8:(9:(10:[])))))))))
-- ==> [1,2,3,4,5,6,7,8,9,10]
```
* lewostronne

```haskell
foldl (+) 0 [1..10]
-- (((((((((0 + 1) + 2) + 3) + 4) + 5) + 6) + 7) + 8) + 9) + 10

let (>:) = flip (:)
foldl (>:) [] [1..10]
-- ((((((((([]>:1)>:2)>:3)>:4)>:5)>:6)>:7)>:8)>:9)>:10
-- ==> [10,9,8,7,6,5,4,3,2,1]
```

### GHCI i statystyki
Aby wyświetlić statystyki zużycia pamięci i czasu wykonania wyrażenia w GHCI trzeba przestawić flagę `:set +s`.

### Zadania

__Zadanie__: Wywołać wymienione wyżej złożenia z włączonymi statystykami

__Zadanie__: Dobrać tak liczby zakresu, aż zaczną pojawia się różnice między lewo- i prawostronnym składaniem

__Zadanie__: Wywołać oba rodzaje złożeń tworzących listę na nieskończonej liście wejściowej

__Zadanie__: Zaimportować funkcję `foldl'` za pomocą `import Data.List (foldl')` i porównać ten wariant z pozostałymi dwoma

---
## Interludium

![SNR](http://www.kessleru.com/wp-content/uploads/2014/07/audiobasics.gif)
