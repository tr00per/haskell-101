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
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f = MaybeT . (fmap (fmap f)) . runMaybeT

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
```

![](http://new4.fjcdn.com/pictures/Cat+racing_d7cac3_4877215.jpg)
