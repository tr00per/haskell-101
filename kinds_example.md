# Typy wyższego rzędu
Tak jak typy służą m. in. weryfikacji wyrażeń, tak rodzaje służą weryfikacji poprawności typów.

```haskell
Int :: *

Maybe     :: * -> *
Maybe Int :: *
```

Można, podobnie jak z funkcjami, stwożyć parametryczne typy wyższego rzędu. Przykład pochodzi z [wykładu Simona Peytona Jonesa](https://www.youtube.com/watch?v=brE_dyedGm0) o typach w Haskellu.

Drzewo "wielogałęziowe" vs. drzewo binarne:
```haskell
data BinTree a = BLeaf a | BNode (Pair (BinTree a))

data RoseTree a = RLeaf a | RNode [RoseTree a]
-- data RoseTree a = RLeaf a | RNode ([] (RoseTree a))

data Pair a = MkPair a a
```

Można wyjąć z nich typ wyższego rzędu:
```haskell
data Tree f a = Leaf a | Node (f (Tree f a))

type RoseTree a = Tree [] a
type BinTree  a = Tree Pair a
type AnnTree  a = Tree AnnPair a

data Pair a = P a a
data AnnPair a = AP String a a
```

* a - typ liścia
* f - konstruktor typu kontenera gałęzi
```haskell
a    :: *
f    :: * -> *
Tree :: (* -> *) -> * -> *
```
