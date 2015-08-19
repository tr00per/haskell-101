# Przykład EDSL dla giełdy
Znalazłem bardzo ładny przykład EDSL w Scali, który przetłumaczyłem do Haskella. Przykład jest uproszczony, jak zaznaczył autor oryginału, a ja sam nie jestem ekspertem od rynków, więc wszelkie niedopowiedzenia i niedosokonałości biorę na siebie.

Źródło: [DSL for the Uninitiated](http://cacm.acm.org/magazines/2011/7/109910-dsl-for-the-uninitiated/fulltext)

```haskell
{-# LANGUAGE ViewPatterns #-}

type Account = String
type Instrument = String
type Money = Integer
type DateTime = (String, String)

newtype Tax = Tax (TaxId, Rational)

data Trade = {
    account    :: Account,
    instrument :: Instrument,
    refNo      :: String,
    market     :: Market,
    unitPrice  :: Money,
    quantity   :: Int,
    tradeDate  :: DateTime,
    valueDate  :: Maybe DateTime,
    taxFees    :: [Tax]
    netAmount  :: Maybe Money
}

data Market = HongKong | Singapore | NewYork | Tokyo

class StockMarket a where
    cashValue :: Trade -> Money

instance StockMarket Market where
    cashValue trade@(market -> HongKong) =
        undefined

    cashValue trade@(market -> Singapore) =
        undefined

    cashValue trade =
        undefined
```
