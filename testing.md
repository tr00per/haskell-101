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
    [(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]
    +++OK,passed100tests.

Also try

    verboseCheck (prop_reverse :: [Int] -> Bool)
