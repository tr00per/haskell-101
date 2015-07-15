# Testing

## QuickCheck
Property-based testing

    import Test.QuickCheck

## Function under test
    reverse :: [a] -> [a]

## Property of the function
    double_reverse xs = reverse (reverse xs) == xs

## Run it
Standard run

    Prelude Test.QuickCheck> quickCheck double_reverse 
    +++ OK, passed 100 tests.

Version for curious

    Prelude Test.QuickCheck> verboseCheck double_reverse
    Passed:
    []
    Passed:
    []
    Passed:
    [()]
    Passed:
    [(),()]
    Passed:
    [(),(),(),()]
    ...
    Passed:
    [(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]
    +++ OK, passed 100 tests.

Also try

    verboseCheck (prop_reverse :: [Int] -> Bool)

## Non-abstract example

    sumAbs = sum . map abs
    prop_sumAbs xs = sumAbs xs == sum xs

It fails, who knew...

    quickCheck prop_sumAbs
    *** Failed! Falsifiable (after 4 tests and 2 shrinks):    
    [-1]
