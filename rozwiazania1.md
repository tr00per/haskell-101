### Rozwiązanie
```haskell
let odd x = x `mod` 2 == 1

let even x = x `mod` 2 == 0

filter odd [1..10]
filter even [1..10]
```

![](http://obeythekitty.com/wp-content/uploads/2015/01/lolcat_airplane.jpg)

### Rozwiązanie
```haskell
let myfilter x = x `mod` 4 == 0 && x `mod` 3 /= 0

filter myfilter [1..20]
```

![](http://dothash.buzz/wp-content/uploads/2015/06/lolcat-4.jpg)

### Rozwiązanie
```haskell
Prelude> foldr (:) [] [1..]
[1,2,3,4,5,6,7,8,9,10,...

Prelude> foldl (>:) [] [1..]
-- ...
```

![](http://images.andrej3000.com/upload/2012/07/20/20120720140325-0002cca8.png)

### Rozwiązanie
```haskell
Prelude> import Data.List

Prelude Data.List> let (>:) = flip (:)

Prelude Data.List> Prelude> foldl' (>:) [] [1..100000000]
[100000000,99999999,99999998,99999997,999999996,99999995,...
(2980.50 secs, 819,459,440,184 bytes)
-- W czasie wypisywania wyników: 69.5% pamięci mojego laptopa

Prelude Data.List> foldr (+) 0 [1..100000000]
*** Exception: stack overflow

Prelude Data.List> foldl' (+) 0 [1..100000000]
5000000050000000
(4.74 secs, 9,601,973,392 bytes)

Prelude Data.List> foldr (+) 0 [1..10000000]
50000005000000
(2.80 secs, 1,555,131,480 bytes)

Prelude Data.List> foldl (+) 0 [1..10000000]
50000005000000
(3.25 secs, 1,536,457,000 bytes)

Prelude Data.List> foldl' (+) 0 [1..10000000]
50000005000000
(0.58 secs, 955,685,392 bytes)
```
![](http://new1.fjcdn.com/pictures/Lolcats_b0a5ec_147272.jpg)

### Rozwiązanie
```haskell
-- sum
sum = foldl' (+) 0

-- product
product = foldl' (*) 0

-- length
length = foldl' (\acc _ -> acc+1) 0

-- ++

-- reverse
reverse

-- take

-- drop

```

![](https://c1.staticflickr.com/9/8217/8361000871_53de696e2d.jpg)
