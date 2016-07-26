# Functional Thinking \#1

All the pleasure comes from expressing ideas in a concise way. From removing noise from our code.

$$
SNR = P_{signal}/P_{noise}
$$

---

## Lists and tuples

In functional programming lists are universally more common than arrays

```haskell
-- a list of numbers
[1,2,3,4,5]

-- "desugared" version
1:2:3:4:5:[]

-- a list of characters
['H','a','s','k','e','l','l']
"Curry" -- in Haskell the String type is just an alias for a list of chars
"Wąchock & 漢字" -- Unicode!

-- Generators
[1..10]
[1,3..10]
```

**Quick exercise**: Create a countdown list of odd numbers between 1 and 100.

**Quick exercise**: Create a list of characters from `'a'` to `'z'`.

All elements of a list must be of the same type. To mix types we need to use a tuple.

```haskell
(10, "Caramel")
(True, -1)
(40, 255, 0)

-- list of tuples - "poor man's dictionary"
[(1,"San Francisco"), (2, "New York")]
```

**Lists** can have zero or more **elements of one type**, while **_tuples_** can **_mix types of elements_**, but once order and length are defined, we cannot change it.

By the way: unit, otherwise known as a singleton type. It's used in Haskell to mark functions that do not return anything useful (they're probably side-effecting and unpure).

```haskell
()
```

Lists are made from a "head" and a "tail":

```haskell
head [1, 2, 3, 4, 5]
-- 1
tail [1, 2, 3, 4, 5]
-- [2,3,4,5]
tail (tail [1, 2, 3, 4, 5])
-- [3, 4, 5]
```

Infinite lists

```haskell
[1..]
```

![To infinity and beyond!](http://img.interia.pl/rozrywka/nimg/2/7/roz4286600.jpg)

### Lazy computation

Try this

```haskell
take 10 [1..]
head [1..]
```

Computation concludes, because only the part that is required by the consuming function is ever created.

**Quick exercise**: Using functions `drop` and `take` obtain a slice of a list between 5th and 10th element. Use an infinite list as the input.

Example: separation of generating potential solutions from actually checking the correctness of a solution:

```haskell
head $ filter correct_solution $ generate_values data
-- or: head (filter correct_solution (generate_values data))
```

---

## Filtering

```haskell
odd 1
even 1

filter odd [1..10]
```

`filter` is a higher-order function, it accepts another function as one of its arguments. In this particular case we call it a predicate.

For comparison in C++:

```cpp
#include <iostream>
#include <list>

std::list<int> filter(bool (*predicate)(int), const std::list<int>& input_list)
{
    std::list<int> output_list;
    for (const int entry : input_list)
    {
        if (predicate(entry))
        {
            output_list.push_back(entry);
        }
    }
    return std::move(output_list);
}

int main()
{
    std::list<int> my_data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto odd = [](int x) -> bool {return x % 2 != 0;};
    std::list<int> filtered_data = filter(odd, my_data);
    for (auto a : filtered_data)
    {
        std::cout << a << ", ";
    }
    std::cout << std::endl;

    return 0;
}
```

![](http://i1.kym-cdn.com/photos/images/original/000/000/681/what-you-did-there-i-see-it.thumbnail.jpg)

Ok, that wasn't entirely fair. Here comes the full application in Haskell:

```haskell
main = print (filter odd [1..10])
```

![](http://i3.kym-cdn.com/entries/icons/original/000/001/987/fyeah.jpg)

A little bit of Scala:

```scala
object Main extends App {
    def odd = (x : Int) => x % 2 != 0
    val filtered = (1 to 10) filter odd
    System.out.println(filtered)
}
```

To be honest, you can filter a collection in C++ by using combination of `remove` and `erase`, but first of all, the predicate is negative, and secondly it a mutating operation, it destroys the underlying collection.

### Custom function in GHCI

```haskell
let myfunc x = x * x
```

For `filter` to accept our function, it must accept a single argument, which type must correspond to the type in the filtered list. The return value must always be of type `Bool`.

**Quick exercise**: Define the `slice` function, which accepts an offset, length and returns a slice of the given list.

### Functions and functions

In Haskell syntactically there are twp types of operations:

* prefix functions \(written with letters, numbers etc.\)

  ```haskell
  myfunc x
  filter myfunc [1..10]
  elem 3 [1..10]
  ```

* infix functions \(operators; written with symbols\)

  ```haskell
  1 + 1
  4.5 * 8
  1:2:[]
  ```

In both cases the default fixity can be changed:

```haskell
(+) 4.5 8
(:) 1 ((:) 2 [])
-- albo (:) 1 $ (:) 2 []

3 `elem` [1..10]
128 `div` 3
```

### GHCI and types

To show the type of an expression in GHCi you can prefix your expression with the `:t` command or toggle a flag to see types of each entered expression with `:set +t`.

### Exercises

**Exercise**: Define your version of functions either `odd` or `even` and use it to filter a list.

**Exercise**: Create a function, which would filter numbers, which are divisible by 4, but not by 3.

Useful functions: `div`, `mod`, `not`, `&&`, `||`, `==`, `/=`.

---

## Mapping

In a functional language, there are no loops.

But we just did a bunch of operations on a list and we did not need a loop!

![Shock!](http://www.pagefield.co.uk/wp-content/uploads/2013/06/shock.jpg)

Another operation on a list, even more popular than filtering, is applying a function to each element of the list. It's called **mapping**.

```C++
double somearr[] = {1, 2, 3, 4, 5}, outarr[5];
for (int i = 0; i < 5; ++i)
{
    outarr[i] = std::sqrt(somearr[i]);
}
```

In Haskell such operation creates a new list.

```haskell
map odd [1..5]
map sqrt [1..5]
map toUpper "Amsterdam" -- wymaga modułu Data.Char
```

C++ \(essence\)

```cpp
std::list<double> somelist = {1, 2, 3, 4, 5}, outlist(5);
std::transform(somelist.begin(), somelist.end(), outlist.begin(), (double(*)(double))std::sqrt);
```

Scala

```scala
(1.0 to 5.0 by 1.0) map (Math.sqrt)
```

**Quick exercise**: Define function `square` and apply it to a list of numbers from 1 to 10.

### Inside the `map`

How could an implementation of the `map` look in Haskell?

```haskell
map f xs = if null xs                         -- null returns True, if the list is empty
           then []                            -- effect of mapping over an empty list is an empty list
           else f (head xs) : map f (tail xs) -- new value becomes a head of the new list, tail is computed recursively
```

Warning: this function is not tail recursive!

---

## Composition

Do you remember function composition from Math class in high school?

$$
f(g(x)) = (f \circ g)(x)
$$

In Haskell we also can compose functions and there's an operator for that!

```haskell
h x = f (g x)
h' x = (f . g) x
```

Just as in mathematics, functional composition in Haskell is read from right to left.

For two function to be eligible for composition, the need to have compatible types in-between.

```haskell
-- (.) :: (b -> c) -> (a -> b) -> a -> c
```

### Composition and mapping

```haskell
let sqr x = x * x
let lessthan8 x = x < 8

:t sqr
-- sqr :: Num a => a -> a
:t lessthan8
-- lessthan8 :: (Num a, Ord a) => a -> Bool

:t (lessthan8 . sqr)
-- (lessthan8 . sqr) :: (Num a, Ord a) => a -> Bool

let transient = map sqr [1..5]
-- [1,4,9,16,25]
map lessthan8 transient
-- [True,True,False,False,False]

map (lessthan8 . sqr) [1..5]
-- [True,True,False,False,False]
```

**Quick exercise**: Define function `cube` and compose it with function `lessthan50`. Use the composed function as a predicate for `filter`.

**Quick exercise**: Compare the result with using the predicate in function `takeWhile` on finite and infinite lists.

---

## Lambda

![](http://vignette1.wikia.nocookie.net/half-life/images/c/c9/Half-Life_Wiki_Logo.png/revision/20130801093040?path-prefix=en)

If you didn't live under a rock for the last few years, you probably heard of lambdas.

Lambdas are functions, which we define ad hoc to avoid naming a trivial function, or simply to avoid naming it improperly.

```haskell
myfilter xs = filter (\x -> x `mod` 4 == 0 && x `mod` 3 /= 3) xs

squares xs = map (\x -> x * x) xs
```

Haskell gives you two more mechanisms for defining local functions, so in "real life" you would see less lambdas, than in textbooks or this part of the workshop.

C++

```cpp
auto sqr = [](int x) -> int {return x * x;};
auto sqr_tpl = [](auto x) -> decltype(x) {return x * x;};
```

Scala

```scala
def sqr = (x : Int) => x * x
def sqr_tpl[A](x: A)(implicit numeric: Numeric[A]): A = numeric.times(x, x)
```

**Quick exercise**: Define the square function as a lambda and apply it to a list of numbers from 1 to 10.

### Jargon and nerding-out

Eta \(η\) conversion - process of adding or removing abstraction over a function.

* Eta-reduction: `\x -> abs x` becomes `abs`
* Eta-abstraction: `abs` becomes `\x -> abs x`

Application of η-reduction is the basis of _pointfree_ style of programming \(_pointless_ if you don't like it\).

```haskell
add''' = (+)
(>:) = flip (:)

h x = f (g x)
h' x = (f . g) x
h'' = f . g
```

It's very useful to define your functions as a pipe of consequent applications of functions. The code becomes more readable, given you keep names of your functions descriptive.

Interesting equations:

```haskell
map f . map g == map (f . g)
map f . filter (p . f) == filter p . map f
```

---

## Currying

![](https://wiki.haskell.org/wikiupload/8/86/HaskellBCurry.jpg)

Currying is a "big thing" in languages such as Scala, because the point of reference is the One True Java function call convention.

```scala
def modN_uncurried(n: Int, x: Int) = x % n == 0
def modN_curried(n: Int)(x: Int) = x % n == 0
```

It comes down to the ability of a function, which accepts a given number of arguments, to accept those arguments one-by-one, effectively creating a series of one-parameter functions. Application of the final argument enables the execution of the main body. Before that last argument we call the function _partially applied_ and the mechanism, enabled by currying, is _partial application_.

In Haskell currying is transparent and partial application is omnipresent.

```haskell
add x y = x + y
add5 y = add 5 

add' = (+)
add5' x = (5+) x

add5'' = add 5
add5''' = (5+)
```

You can think of all the function in Haskell as a series of one-argument functions.

```haskell
add x y = x + y
add' x = \y -> x + y
add'' = \x -> \y -> x + y
```

### Jargon and nerding-out

Currying was really created by a Russian mathematician and creator of the combinatory logic: Moses Schönfinkel. Haskell Curry, who was an American, expanded Schönfinkel's concept.

![](https://upload.wikimedia.org/wikipedia/commons/9/97/Schonfinkel.gif)

### A bit closer to reality

Dependency injection with partial application

```haskell
-- withdraw :: Num a => (a -> a -> Bool) -> a -> a -> a
withdraw policy available requested =
    if policy available requested
    then available - requested
    else available

-- simplePolicy :: Ord a => a -> a -> Bool
simplePolicy has wants = has >= wants

-- bank1Withdraw :: (Num a, Ord a) => a -> a -> a
bank1Withdraw = withdraw simplePolicy
```

---

## Folds

There's one more higher-order function for today, one that is of even higher abstraction than `filter` or `map`. It encapsulates the idea of reducing the container (e.g., list) into a result given a starting point and the method.

```haskell
map f xs = if null xs
           then []
           else f (head xs) : map f (tail xs)

filter p xs = if null xs
              then []
              else pred
    where pred = if p (head xs) then (head xs) : filter p (tail xs) else filter p (tail xs)

map f xs = foldr (???) [] xs             -- this will be an exercise

filter p xs = foldr pred [] xs
    where pred x acc = if p x then x:acc else acc
```

Folding in Haskell comes in two flavours

* right fold

```haskell
foldr (\x acc -> x + acc) 0 [1..10]
-- 0 + (1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + 10)))))))))

foldr (\x acc -> x : acc) [] [1..10]
-- 1:(2:(3:(4:(5:(6:(7:(8:(9:(10:[])))))))))
-- ==> [1,2,3,4,5,6,7,8,9,10]
```

* left fold

```haskell
foldl (\acc x -> acc + x) 0 [1..10]
-- (((((((((0 + 1) + 2) + 3) + 4) + 5) + 6) + 7) + 8) + 9) + 10

foldl (\acc x -> x : acc) [] [1..10]
-- 10:(9:(8:(7:(6:(5:(4:(3:(2:(1:[])))))))))
-- ==> [10,9,8,7,6,5,4,3,2,1]
```

### GHCI and stats

To display memory and execution time statistics in GHCi toggle a flag `:set +s`.

### Exercises

**Exercise**: Implement a few Prelude function using a fold of your choosing: `sum` (or `product`), `length`, `map`. While defining the `map` try using function compisition with the list-creating function `(:)`.

---

## Pattern matching and guards

Ignoring for a while that `map` is really implemented in terms of `fold`, we saw the definition as shown below:

```haskell
map f xs = if null xs
           then []
           else f (head xs) : map f (tail xs)
```

There is a bettern method:

```haskell
map f []     = []
map f (x:xs) = f x : map f xs
```

Or even better:

```haskell
map _ []     = []
map f (x:xs) = f x : map f xs
```

The `_` in the pattern means, that we won't use the argument on this position.

Pattern matching is a common mechanism in Haskell. One can also write it inside the function body as a `case` expression:

```haskell
map f xs = case xs of
               []   -> []
               x:xs -> f x : map f xs
```

Additionally we can use guards to ensure some further conditions:

```haskell
legal 0             = False
legal x | x < -5    = False
        | x > 0     = True
        | otherwise = True
```

It also can be used inside the function body:

```haskell
legal x = case x of
              0             -> False
              x | x < -5    -> False
                | x > 0     -> True
                | otherwise -> True
```

### Exercises

**Exercise**: Implement a few Prelude function using pattern matching and guards: `(++)`, `reverse`, `take` (or `drop`\).

---

## Interludium

![SNR](http://www.kessleru.com/wp-content/uploads/2014/07/audiobasics.gif)
