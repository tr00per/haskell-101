# Testing

## QuickCheck
Property-based testing
```haskell
import Test.QuickCheck
```

#### Function under test
```haskell
reverse :: [a] -> [a]
```

### Property of the function
```haskell
double_reverse xs = reverse (reverse xs) == xs
```

### Run it
Standard run
```haskell
Prelude Test.QuickCheck> quickCheck double_reverse 
+++ OK, passed 100 tests.
```

Version for curious
```haskell
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
```

Also try
```haskell
verboseCheck (prop_reverse :: [Int] -> Bool)
```

### Non-abstract example
```haskell
sumAbs = sum . map abs
prop_sumAbs xs = sumAbs xs == sum xs
```

It fails, who knew...
```haskell
quickCheck prop_sumAbs
*** Failed! Falsifiable (after 4 tests and 2 shrinks):    
[-1]
```

## HUnit

## Tasty
