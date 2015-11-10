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

```haskell
newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

fmapMT :: (Functor m) => (a -> b) -> MaybeT m a -> MaybeT m b

pureMT :: (Applicative m) => a -> MaybeT m a

apMT :: (Applicative m) => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b

failMT :: (Monad m) => t -> MaybeT m a
```

![](http://new4.fjcdn.com/pictures/Cat+racing_d7cac3_4877215.jpg)
