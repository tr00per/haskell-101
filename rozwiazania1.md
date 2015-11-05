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
-- sum
sum = foldl' (+) 0

-- product
product = foldl' (*) 1

-- length
length = foldl' (\acc _ -> acc + 1) 0

-- map
map f = foldr (\x acc -> f x : acc) []

map f = foldr ((:) . f) []
```

![](http://new1.fjcdn.com/pictures/Lolcats_b0a5ec_147272.jpg)

### Rozwiązanie
```haskell
-- (++)
[] ++ ys = ys
xs ++ [] = xs
(x:xs) ++ ys = x:xs ++ xy

-- reverse
reverse [] = []
reverse (x:xs) = xs ++ [x]

-- take
take _ [] = []
take n (x:xs) | n <= 0    = []
              | otherwise = x : take (n-1) xs

-- drop
drop _ [] = []
drop n xxs@(_:xs) | n <= 0    = xxs
                  | otherwise = drop (n-1) xs
```

![](https://c1.staticflickr.com/9/8217/8361000871_53de696e2d.jpg)
