# Haskell 101

## Haskell is...

* Functional (functions are first-class entities)
* Strong- and Statically-typed
* Lazy
* Pure

## Environment
### GHCI
Interactive console! (REPL - Read Evaluate Print Loop)

Useful commands:
* `:set +t` - print type after evaluation
* `:set +s` - print time/memory stats after evaluation

To declare something in GHCI, you have to prefix it with `let`
```haskell
let x = 2
```

### Compilation
```bash
ghc Main.hs
```

### Almost a script
```bash
runghc Main.hs
```

## Type system
### Basics
Values
```haskell
2 :: Int
[1,2,3] :: [Int]
"abc" :: [Char]
4:5:6:[] :: [Int]
['a', 'b', 'c'] :: [Char]
(1, 'a', True) :: (Int, Char, Bool)
```

Functions
```haskell
-- one argument
sumInts :: [Int] -> Int

-- two arguments
add :: Int -> Int -> Int
```

### Advanced
Generic functions
```haskell
length :: [a] -> Int
reverse :: [a] -> [a]
```

Generic addition in C++
```c++
template <typename T> T add(T x, T y) {
    return x + y;
}
```

Generic addition in Haskell
```haskell
add :: Num a => a -> a -> a
add x y = x + y
```

Constraints (context)
```haskell
find :: Eq a => a -> [a] -> a
sort :: Ord a => [a] -> [a]
```

## Function calls
### Regular
```haskell
sum [1, 2, 3]
add 4 (-5)
(+) 6 1
head "abc"
tail "abc"
```

### Operators
```haskell
2 - 4
3.4 / 2
6 `add` 1
```

### Lambdas
```haskell
square = \x -> x * x
```
```haskell
Prelude> let square = \x -> x * x
Prelude> :t square
square :: Num a => a -> a
```

### Currying, partial application
```haskell
add x y = x + y
add' x = \y -> x + y
add'' = \x -> \y -> x + y
-- add, add', add'' :: Int -> Int -> Int

add5 = add 5
realAdd = (+)
```

### Functions everywhere
Collection of functions
```haskell
Prelude> :t [add'', (+)]
[add'', (+)] :: Num a => [a -> a -> a]
```

Higher-order functions
```haskell
Prelude> filter odd [1,2,3,4,5]
[1,3,5]
Prelude> :t filter
filter :: (a -> Bool) -> [a] -> [a]
```

```haskell
Prelude> zip [1,3..100] "abcdef"
[(1,'a'),(3,'b'),(5,'c'),(7,'d'),(9,'e'),(11,'f')]
Prelude> :t zip
zip :: [a] -> [b] -> [(a, b)]
```
## Laziness
Infinite lists
```haskell
[1..]
[1,3..]
```
```haskell
Prelude> take 3 [1..]
[1,3,5]
```

## Function chaining
```haskell
Prelude> take 3 (filter odd [1..])
[1,3,5]
Prelude> take 3 $ filter odd [1..]
[1,3,5]
Prelude> (take 3 . filter odd) [1..]
[1,3,5]
```

## Pattern matching
```haskell
take 0 _  = []
take _ [] = []
take n (x:xs) = x:take (n-1) xs
```

## Gaurds
```haskell
take _ [] = []
take n (x:xs)
    | n <= 0    = []
    | otherwise = x:take (n-1) xs
```

## List comprehension
```haskell
oddNaturals = [ x | x <- [1..], odd x ]

```

### Mandatory Fibonacci sequence
```haskell
fib = 0:1:[ x+y | (x,y) <- zip fib (tail fib) ]
```
