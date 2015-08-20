### Rozwiązanie
```haskell
import Data.Char

upper :: String -> String
upper = map toUpper

upperIdent :: String -> Ident String
upperIdent x = return (upper x)

main = do
    print (Ident 42)
    print $ upper `fmap` (Ident "abc")
    print $ (+) <$> (Ident 3) <*> (Ident 2)
    print $ return "def" >>= upperIdent

data Ident a = Ident a deriving Show

instance Functor Ident where
    fmap f (Ident x) = Ident (f x)

instance Applicative Ident where
    pure x = Ident x
    (Ident f) <*> (Ident x) = Ident (f x)

instance Monad Ident where
    (Ident x) >>= f = f x
```

![](http://www.timemachinego.com/linkmachinego/wordpress/wp-content/uploads/2009/02/techcatpreview.jpg)
