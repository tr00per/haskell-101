# Haskell 101


## What is Haskell

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
Polymorphic functions
```haskell
length :: [a] -> Int
reverse :: [a] -> [a]
```

Constraints (context)
```haskell
find :: Eq a => a -> [a] -> a
sort :: Ord a => [a] -> [a]
```

## Function calls
Regular
```haskell
sumInts [1, 2, 3]
add 4 (-5)
(+) 6 1
```

Operators
```haskell
2 - 4
3.4 / 2
6 `add` 1
```

Lambdas
```haskell
square = \x -> x * x
```
```haskell
Prelude> let square = \x -> x * x
Prelude> :t square
square :: Num a => a -> a
```

Currying, partial application
```haskell
add x y = x + y
add' x = \y -> x + y
add'' = \x -> \y -> x + y
-- add, add', add'' :: Int -> Int -> Int

add5 = add 5
```

## Functions everywhere
Collection of functions
```haskell
Prelude> :t [add'', (+)]
[add'', (+)] :: Num a => [a -> a -> a]
```

Higher-order functions
```haskell
Prelude> filter odd [1,2,3,4,5]
[1,3,5]
```

## Laziness
Infinite lists
```haskell
Prelude> [1..]
```
```haskell
Prelude> take 3 [1..]
[1,3,5]
```

## Everything together
```haskell
Prelude> take 3 (filter odd [1..])
[1,3,5]
Prelude> take 3 $ filter odd [1..]
[1,3,5]
```
