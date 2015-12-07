# Embeded Domain-Specific Language
## Co to?
Z EDSL mamy do czynienia wtedy, kiedy wykorzystując elementy języka tworzymy w nim zestaw narzędzi, którymi łatwiej jest operować w danej dziedzinie wiedzy. Idea jest taka, żeby analityk biznesowy mógł razem z programistą stworzyć wspólny język, który będzie zrozumiały dla obu stron. Kompilowane zasady biznesowe.

Jednym podejściem jest stworzenie lub dołączenie zewnętrznego języka skryptowego. 

Przykładem zewnętrznego języka domenowego, o którym prawdopodobnie wszyscy słyszeliście, jest **SQL**. Jest to język domenowy do manipulacji obszernymi zbiorami danych z bazie relacynej.

Można tez pod tę definicję podciągnąć języki skryptowe używane w grach do programowania zachowań przeciwników (np. wybór najlepszej ścieżki; sam algorytm jej wyznaczania będzie zaimplementowany w głównym języku) lub modelowania zachowań środowiska (np. kiedy ma zacząć padać deszcz). **Lua** jest przykładem języka, który został zastosowany w wielu grach własnie do tego celu (Wikipedia wymienia 151 gier, które go wykorzystują, m. in.: Angry Birds, Baldur's Gate, Freeciv, Saints Row 2/3/4, Warhammer 40k: Dawn of War 1/2).

Jeśli język oferuje wystarczającą ekspresywność, można ominąć wprowadzenie zwenętrznego języka (i pisanie parsera albo wciąganie frameworku, jak [xText](https://eclipse.org/Xtext/) dla Javy).

## Przykład EDSL dla jednostek
Przykład poniżej zapożyczyłem z książki [Real World Haskell](http://book.realworldhaskell.org/read/), a reprezentuje on EDSL wspomagający obliczenia za pomocą implementacji matematyki jednostek

Przykład w działaniu:
```haskell
ghci> ((units 50 "m") * sin (units 90 "deg")) :: Units (SymbolicManip Double)
50.0*sin(((2.0*pi)*90.0)/360.0)_m
ghci> prettyShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
"50.0*sin(((2.0*pi)*90.0)/360.0)"
ghci> rpnShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
"50.0 2.0 pi * 90.0 * 360.0 / sin *"
ghci> (units (Symbol "x") "m") * sin (units 90 "deg")
x*sin(((2.0*pi)*90.0)/360.0)_m
```

Kod implementujący obliczenia (uwaga, bogaty przykład; ch13/num.hs)
```haskell
import Data.List

--------------------------------------------------
-- Symbolic/units manipulation
--------------------------------------------------

-- The "operators" that we're going to support
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

{- The core symbolic manipulation type.  It can be a simple number,
a symbol, a binary arithmetic operation (such as +), or a unary
arithmetic operation (such as cos)

Notice the types of BinaryArith and UnaryArith: it's a recursive
type.  So, we could represent a (+) over two SymbolicManips. -}
data SymbolicManip a = 
          Number a           -- Simple number, such as 5
        | Symbol String      -- A symbol, such as x
        | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
        | UnaryArith String (SymbolicManip a)
          deriving (Eq)

{- SymbolicManip will be an instance of Num.  Define how the Num
operations are handled over a SymbolicManip.  This will implement things
like (+) for SymbolicManip. -}
instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)

{- Make SymbolicManip an instance of Fractional -}
instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

{- Make SymbolicManip an instance of Floating -}
instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a

{- Show a SymbolicManip as a String, using conventional
algebraic notation -}
prettyShow :: (Show a, Num a) => SymbolicManip a -> String

-- Show a number or symbol as a bare number or serial
prettyShow (Number x) = show x
prettyShow (Symbol x) = x

prettyShow (BinaryArith op a b) =
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
        in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) = 
    opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

{- Add parenthesis where needed.  This function is fairly conservative
and will add parenthesis when not needed in some cases.
    
Haskell will have already figured out precedence for us while building
up the SymbolicManip. -}
simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

{- Showing a SymbolicManip calls the prettyShow function on it -}
instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow a

{- Show a SymbolicManip using RPN.  HP calculator users may
find this familiar. -}
rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i =
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op a b) = toList a ++ toList b ++
           [op2str op]
        toList (UnaryArith op a) = toList a ++ [op]
        join :: [a] -> [[a]] -> [a]
        join delim l = concat (intersperse delim l)
    in join " " (toList i)

{- Perform some basic algebraic simplifications on a SymbolicManip. -}
simplify :: (Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) = 
    let sa = simplify ia
        sb = simplify ib
        in
        case (op, sa, sb) of 
                (Mul, Number 1, b) -> b
                (Mul, a, Number 1) -> a
                (Mul, Number 0, b) -> Number 0
                (Mul, a, Number 0) -> Number 0
                (Div, a, Number 1) -> a
                (Plus, a, Number 0) -> a
                (Plus, Number 0, b) -> b
                (Minus, a, Number 0) -> a
                _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

--------------------------------------------------
-- Units of measure support
--------------------------------------------------
{- New data type: Units.  A Units type contains a number
and a SymbolicManip, which represents the units of measure.
A simple label would be something like (Symbol "m") -}
data Num a => Units a = Units a (SymbolicManip a)
           deriving (Eq)

{- Implement Units for Num.  We don't know how to convert between
arbitrary units, so we generate an error if we try to add numbers with
different units.  For multiplication, generate the appropriate
new units. -}
instance (Num a) => Num (Units a) where
    (Units xa ua) + (Units xb ub) 
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add or subtract"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

{- Make Units an instance of Fractional -}
instance (Fractional a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)

{- Floating implementation for Units.

Use some intelligence for angle calculations: support deg and rad
-}
instance (Floating a) => Floating (Units a) where
    pi = (Units pi (Number 1))
    exp _ = error "exp not yet implemented in Units"
    log _ = error "log not yet implemented in Units"
    (Units xa ua) ** (Units xb ub) 
        | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua) 
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "Units for sin must be deg or rad"
    cos (Units xa ua) 
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "Units for tan must be deg or rad"
    asin (Units xa ua) 
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "Units for acos must be empty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "Units for atan must be empty"
    sinh = error "sinh not yet implemented in Units"
    cosh = error "cosh not yet implemented in Units"
    tanh = error "tanh not yet implemented in Units"
    asinh = error "asinh not yet implemented in Units"
    acosh = error "acosh not yet implemented in Units"
    atanh = error "atanh not yet implemented in Units"

{- A simple function that takes a number and a String and returns an
appropriate Units type to represent the number and its unit of measure -}
units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)

{- Extract the number only out of a Units type -}
dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x
                                                    
{- Utilities for the Unit implementation -}
deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)

{- Showing units: we show the numeric component, an underscore,
then the prettyShow version of the simplified units -}
instance (Show a, Num a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)
```

---
## Przykład EDSL dla giełdy
Znalazłem bardzo ładny choć ograniczony przykład EDSL w Scali, który przetłumaczyłem do Haskella. Sam nie jestem ekspertem od rynków, więc wszelkie niedopowiedzenia i niedosokonałości musicie mi wybaczyć.

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
