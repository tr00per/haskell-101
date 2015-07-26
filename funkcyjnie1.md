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

Filtrowanie jest bardzo popularną operacją na liście elementów. Równie powszechną, jeśli nie wszechobecną, operacją jest transformacja wartości w liście na nowe wartości. W Haskellu taka operacja wytwarza nową listę.
```haskell
map odd [1..5]
```

## Łączenie

---

## Składanie
Jest jeszcze jedna operacja wyższego rzędu, którą warto przyswoić, ponieważ znajduje się na jeszcze wyższym poziomie abstrakcji, niż dwie poprzednie.

Składanie, występuje w dwóch odmianach
* lewostronne
```haskell
foldl (+) [1
```
* prawostronne
```haskell
```

Za pomocą składania można wyrazić obie poprzednie funkcje
```haskell
map f xs = foldr ((:) . f) [] xs
```

### GHCI i statystyki
Aby wyświetlić statystyki zużycia pamięci i czasu wykonania wyrażenia w GHCI trzeba przestawić flagę `:set +s`.

### Leniwe obliczanie

---

## Lambda

---

## Currying
Currying to "wielka rzecz" w językach takich jak Scala, ponieważ odnosi się ją do jedynej słusznej konwencji wywołania funkcji w Javie.

Chodzi o to, że funckję, która przyjmuje ustaloną liczbę elementów, można zamienić na serię funkcji, które przyjmują tylko jeden argument. Pojęcie to łączy się z _częściową aplikacją_. W Haskellu częściowa aplikacja jest powszechnie używana.
```haskell
map f xs = foldr ((:) . f) [] xs
map' f = foldr ((:) . f) []
```

---

## Podsumowanie
Zasada pojedyńczej odpowiedzialności i utrzymywania krótkich blokóœ kodu są uniwersalnymi zasadami. Brak szumu pozwala skupić się na istotnych aspektach.

![SNR](http://www.kessleru.com/wp-content/uploads/2014/07/audiobasics.gif)
