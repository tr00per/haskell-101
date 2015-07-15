# Haskell 101


## What is Haskell


* Functional (functions are first-class entities)
* Strong- and Statically-typed
* Lazy
* Pure

## GHCI
Interactive console!
Useful commands:
* `:set +t` - print type after evaluation
* `:set +s` - print time/memory stats after each evaluation

## Type system
Basics
```haskell
-- a value
days :: Int

-- function with one argument
sumInts :: [Int] -> Int

-- function with two arguments
add :: Int -> Int -> Int
```

Advanced
```haskell
-- polymorphic function
reverse :: [a] -> [a]

-- constraints (context)
find :: (Eq a) => [a] -> a
```

## Function calls
Common
Operators
Lambdas
Currying

## Error handling
Maybe
Either
