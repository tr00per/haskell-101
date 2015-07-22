# Myśleć funkcyjnie #1
Cała przyjemność polega na tym, żeby małą ilością kodu wyrazić możliwie dużo. Na usuwaniu szumu z kodu.

![SNR](https://upload.wikimedia.org/math/f/0/e/f0e032777062c3f945554f1c63d9c864.png)

## Listy (i krotki)
W programowaniu funkcyjnym powszechnie używa się list zamiast tablic
```haskell
-- Lista liczb
[1,2,3,4,5]

-- Lista znaków
['H','a','s','k','e','l']
"Curry" -- w Haskellu typ String jest aliasem dla tablicy znaków
"Wąchock & 漢字" -- Unicode!

-- Generatory
[1..10]
[1,3..10]
```
Wszystkie elementy w liście muszą być tego samego typu. Aby móc mieszać typy można użyć krotki
```haskell
(10, "Lukrecja")
(True, -1)
(40, 255, 0)

-- lista krotek
[(1,1), (1,2)]
```
Unit, czyli bezwartościowa wartość
```haskell
()
```
![To infinity and beyond!](http://img.interia.pl/rozrywka/nimg/2/7/roz4286600.jpg)
```haskell
[1..]
```
## Filtrowanie
```haskell
[1,3..10]

odd 1
even 1

filter odd [1..10]
```
`filter` jest funkcją wyższego rzędu, przyjmuje jako jeden ze swoich argumentów inną funkcję!

Definiowanie własnej funkcji (w GHCI)
```haskell
let myfunc x = x * x
```

__Zadanie #1__: napisać funckję, która posłuży do odfiltrowania liczb, które są podzielne przez 4

__Zadanie #2__: napisać funckję, która posłuży do odfiltrowania liczb, które są podzielne przez 4, ale nie przez 3

Przydatne funkcje: `div`, `mod`, `&&`, `||`, `==`, `\=`. Zdefiniowane funkcje muszą przyjmować jeden argument, którego typ musi zgadzać się z typem przechowywanym w liście. Wartością zwracaną musi być `Bool`.

## Mapowanie
## Składanie
## Lambda
## Currying
## Łączenie
## Podsumowanie
Zasada pojedyńczej odpowiedzialności i utrzymywania krótkich blokóœ kodu są uniwersalnymi zasadami. Brak szumu pozwala skupić się na istotnych aspektach.

![SNR](http://www.kessleru.com/wp-content/uploads/2014/07/audiobasics.gif)
