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

## Hello world
    main = putStrLn "Hello, world!"

## Type system
Basics

    -- a value
    days :: Int

    -- function with one argument
    sumInts :: [Int] -> Int

    -- function with two arguments
    add :: Int -> Int -> Int

Advanced

    -- polymorphic function
    reverse :: [a] -> [a]

    -- constraints (context)
    find :: (Eq a) => [a] -> a

## Function calls
Common
Operators
Lambdas
Currying

## Life without loops
Tail recursion
`map`, `filter`, `fold`
