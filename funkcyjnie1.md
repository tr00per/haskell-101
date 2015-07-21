# Myśleć funkcyjnie #1
Cała przyjemność polega na tym, żeby małą ilością kodu wyrazić możliwie dużo. Na usuwaniu szumu z kodu.

![SNR](http://www.kessleru.com/wp-content/uploads/2014/07/audiobasics.gif)

## Listy
W programowaniu funkcyjnym powszechnie używa się list zamiast tablic
```haskell
-- Lista liczb
[1,2,3,4,5]

-- Lista znaków
['H','a','s','k','e','l']
"Curry" -- w Haskellu typ String jest aliasem dla tablicy znaków
"Wąchock" -- w UTF-8!

-- Generatory
[1..10]
[1,3..10]
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

## Mapowanie
## Składanie
## Lambda
## Currying
## Łączenie
## Podsumowanie
Zasada pojedyńczej odpowiedzialności i utrzymywania krótkich blokóœ kodu są uniwersalnymi zasadami. Brak szumu pozwala skupić się na istotnych aspektach.

![SNR](http://www.kessleru.com/wp-content/uploads/2014/07/audiobasics.gif)
