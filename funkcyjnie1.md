# Myśleć funkcyjnie #1
Cała przyjemność polega na tym, żeby małą ilością kodu wyrazić możliwie dużo. Na usuwaniu szumu z kodu.

$$SNR = P_{signal}/P_{noise}$$

## Listy i krotki
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
(10, "Karmel")
(True, -1)
(40, 255, 0)

-- lista krotek
[(1,1), (1,2)]
```
**Listy** mogą przechowywać zero albo więcej **elementów jednego typu**, natomiast *krotki* mogą *mieszać typy elementów*, jednak raz zdefiniowanych kolejności i rozmiaru nie da sie zmienić

Przy okazji: unit, czyli bezwartościowa wartość
```haskell
()
```
Nieskończone listy
```haskell
[1..]
```

![To infinity and beyond!](http://img.interia.pl/rozrywka/nimg/2/7/roz4286600.jpg)

---

## Filtrowanie
```haskell
[1,3..10]

odd 1
even 1

filter odd [1..10]
```
`filter` jest funkcją wyższego rzędu, przyjmuje jako jeden ze swoich argumentów inną funkcję!

Definiowanie własnej funkcji w GHCI
```haskell
let myfunc x = x * x
```

Aby `filter` zaakceptował naszą funkcję, musi ona przyjmować jeden argument, którego typ musi zgadzać się z typem przechowywanym w liście. Wartością zwracaną musi być typu `Bool`.

### GHCI i typy

Aby wyświetlić typ wyrażenia w GHCI trzeba poprzedzić je komendą `:t` albo przestawić flagę `:set +t`.



### Zadania

__Zadanie #1__: Zdefiniować własną funckję `odd` albo `even` i użyć jej do przefiltrowania listy.

__Zadanie #2__: Napisać funckję, która posłuży do odfiltrowania liczb, które są podzielne przez 4, ale nie przez 3

Przydatne funkcje: `div`, `mod`, `&&`, `||`, `==`, `\=`. 

---

## Mapowanie
W językach funkcyjnych nie ma pętli.

A przed chwilą wykonaliśmy serię operacji na liście wartości i wcale jej nam nie brakowało!

![Shock!](http://www.pagefield.co.uk/wp-content/uploads/2013/06/shock.jpg)

## Składanie

---

## Lambda

---

## Currying

---

## Łączenie

---

## Podsumowanie
Zasada pojedyńczej odpowiedzialności i utrzymywania krótkich blokóœ kodu są uniwersalnymi zasadami. Brak szumu pozwala skupić się na istotnych aspektach.

![SNR](http://www.kessleru.com/wp-content/uploads/2014/07/audiobasics.gif)
