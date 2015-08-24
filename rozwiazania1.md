### Rozwiązanie
```haskell
let odd x = x `mod` 2 == 1

let even x = x `mod` 2 == 0

filter odd [1..10]
filter even [1..10]
```

### Rozwiązanie
```haskell
let myfilter x = x `mod` 4 == 0 && x `mod` 3 /= 0

filter myfilter [1..20]
```

### Rozwiązanie
```haskell
Prelude> foldr (+) 0 [1..10]
55
(0.03 secs, 6,624,720 bytes)

Prelude> foldr (:) [] [1..10]
[1,2,3,4,5,6,7,8,9,10]
(0.01 secs, 0 bytes)

Prelude> foldl (+) 0 [1..10]
55
(0.00 secs, 0 bytes)

Prelude> foldl (>:) [] [1..10]
[10,9,8,7,6,5,4,3,2,1]
(0.00 secs, 0 bytes)
```

### Rozwiązanie
```haskell
Prelude> let (>:) = flip (:)

Prelude> foldr (:) [] [1..10000]
[1,2,3,4,5,6,7,8,9,10,...
(0.18 secs, 49,959,032 bytes)

Prelude> foldl (>:) [] [1..10000]
[10000,9999,9998,9997,9996,9995,...
(0.18 secs, 47,884,840 bytes)

Prelude> foldr (:) [] [1..100000000]
[1,2,3,4,5,6,7,8,9,10,...

Prelude> foldl (>:) [] [1..100000000]
Unicestwiony
```

### Rozwiązanie
```haskell
Prelude> foldr (:) [] [1..]
[1,2,3,4,5,6,7,8,9,10,...

-- W czasie wypisywania wyników: 0.2% pamięci mojego laptopa

Prelude> foldl (>:) [] [1..]
-- ...
```

### Rozwiązanie
```haskell
Prelude> import Data.List

Prelude Data.List> let (>:) = flip (:)

Prelude Data.List> Prelude> foldl' (>:) [] [1..100000000]
[100000000,99999999,99999998,99999997,999999996,99999995,...

-- W czasie wypisywania wyników: 69.5% pamięci mojego laptopa
```
