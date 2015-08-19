# Przykład EDSL dla giełdy
Znalazłem bardzo ładny przykład EDSL w Scali, który przetłumaczyłem do Haskella. Przykład jest uproszczony, jak zaznaczył autor oryginału, a ja sam nie jestem ekspertem od rynków, więc wszelkie niedopowiedzenia i niedosokonałości biorę na siebie.

Źródło: [DSL for the Uninitiated](http://cacm.acm.org/magazines/2011/7/109910-dsl-for-the-uninitiated/fulltext)

```haskell
{-# LANGUAGE ViewPatterns #-}

type Account = String
type Instrument = String
type Money = Integer
type DateTime = (String, String)
type TaxId = Int

newtype Tax = Tax (TaxId, Rational)

data Trade = Trade {
    getAccount    :: Account,
    getInstrument :: Instrument,
    getRefNo      :: String,
    getMarket     :: Market,
    getUnitPrice  :: Money,
    getQuantity   :: Int,
    getTradeDate  :: DateTime,
    getValueDate  :: Maybe DateTime,
    getTaxFees    :: [Tax],
    getNetAmount  :: Maybe Money
}

data Market = HongKong | Singapore | NewYork | Tokyo

class RealTrade a where
    market :: a -> Market

class StockMarket a where
    cashValue :: RealTrade a => a -> Money

instance RealTrade Trade where
    market = getMarket

instance StockMarket Trade where
    cashValue trade@(market -> HongKong) =
        let qty   = fromIntegral $ getQuantity trade
            price = getUnitPrice trade 
        in qty * price

    cashValue trade@(market -> Singapore) =
        undefined

    cashValue trade =
        undefined

main :: IO ()
main = print $ cashValue
    (Trade "123" "A" "AV2/3" HongKong 10 2 ("2015-08-19","10:03:44") Nothing [] Nothing)
```
