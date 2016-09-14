Data definition for a Binary Tree, known as BinTree. 
The tree type can be empty or can have a node that 
holds a value and also holds two other binary trees

> data BinTree a = Empty | Node a (BinTree a) (BinTree a)
>                  deriving (Eq,Show)

Helper functions to create empty trees with a node value and 
create trees with other trees

> emptyBinTree :: a -> BinTree a
> emptyBinTree a = Node a Empty Empty

> newBinTree :: a -> BinTree a -> BinTree a -> BinTree a
> newBinTree a bt1 bt2= Node a bt1 bt2

Checks whether the tree t contains a node with a label x.
Runs down the tree checking each branch, if an elem matches 
returns true, false if it finds an empty leaf.

> hasbt :: Eq a => a -> BinTree a -> Bool
> hasbt x Empty = False
> hasbt x (Node a bt1 bt2)
>      | x == a = True
>      | otherwise = hasbt x bt1 || hasbt x bt2

Test whether trees t1 and t2 are identical; i.e. are 
both empty, or have the same label at the root and
the same subtrees.

> equalbt :: Eq a => BinTree a -> BinTree a -> Bool
> equalbt Empty Empty = True
> equalbt (Node a bt1 bt2) (Node b bt3 bt4)
>        | a == b = (equalbt bt1 bt3) && (equalbt bt2 bt4)
>        | otherwise = False
> equalbt _ _ = False

Construct a mirror image of tree t. Simply swaps the 
nodes on the tree then recursively calls down to each node

> reflectbt :: BinTree a -> BinTree a
> reflectbt Empty = Empty
> reflectbt (Node a btl btr) = Node a (reflectbt btr) (reflectbt btl)

Construct the fringe of tree t; i.e. a list containing 
the labels on the leaves of the tree, in the order they 
would be visited in a left-to-right depth-first traversal.
Runs through the tree, if a node has empty children then 
its value is added to the list, one or both children may 
be empty to be considered a leaf node.

> fringebt :: Eq a => BinTree a -> [a]
> fringebt Empty = []
> fringebt t = fringebt' t []

> fringebt' :: Eq a => BinTree a -> [a] -> [a]
> fringebt' (Node n btl btr) xs
>          | btl == Empty && btr /= Empty = fringebt' btr (xs ++ [n])
>          | btl /= Empty && btr == Empty = fringebt' btl (xs ++ [n])
>          | btl == Empty && btr == Empty = (xs ++ [n])
>          | otherwise = (fringebt' btl xs) ++ (fringebt' btr xs)

Check whether tree t is full; i.e. if every node has 
either 0 or 2 subtrees. Runs through each node on the
tree, and performs what pretty much equates to a not 
exclusive or check, if both nodes are empty then it 
returns true, if both are full it recurses through 
them, if one is empty it must be false.

> fullbt :: Eq a => BinTree a -> Bool
> fullbt (Node a btl btr)
>       | btl == Empty && btr == Empty = True
>       | btl == Empty && btr /= Empty = False
>       | btl /= Empty && btr == Empty = False
>       | otherwise = fullbt btl && fullbt btr
> fullbt Empty = True

empty : Returns an empty BST, not much to it. Does 
require an argument. I could rewrite the BinTree definition 
to allow for the lack of an argument using Maybe.

> empty :: a -> BinTree a
> empty a = Node a Empty Empty

insert : Inserts an item into the appropriate place 
in a given BST, unless it's already present. ie if it 
finds a leaf node where an element should be placed then 
it returns the tree with the element in place, if it finds 
the element within the tree then it simply returns the 
tree as is. Until either of those cases is fulfilled it 
searches for the appropriate position relative to the 
value of the current node.

> insert :: Ord a => a -> BinTree a -> BinTree a
> insert a Empty = Node a Empty Empty
> insert a (Node n btl btr)
>       | n == a = Node n btl btr
>       | n < a = Node n btl (insert a btr)
>       | n > a = Node n (insert a btl) btr

has : Checks whether a given item occurs in a BST, 
same idea as the hasbt function above. Searches for the 
appropriate position in the tree, if found then it returns 
true, if not and it runs out of tree to search then by 
default it returns false.

> has :: Ord a => a -> BinTree a -> Bool
> has a (Node n btl btr)
>    | a == n = True
>    | a < n = has a btl
>    | a > n = has a btr

delete : Deletes a given item from the BST if it's 
present. Finds the position of the item, assuming it's 
there in the first place, if it isn't it runs out of 
tree to search and returns the original tree, if found 
it passes the tree to another function, which determines 
which subtree to put in its place, either the left child 
or the leftmost child of the right child. When the 
leftmost child is pushed up it is also deleted from the 
subtree so that deleting the root doesn't also leave 
behind the node.

> delete :: Ord a => a -> BinTree a -> BinTree a
> delete _ Empty = Empty
> delete a (Node n btl btr)
>       | a == n = delete' (Node n btl btr)
>       | a < n = Node n (delete a btl) btr
>       | a > n = Node n btl (delete a btr)

> delete' :: Ord a => BinTree a -> BinTree a
> delete' (Node n Empty btr) = btr
> delete' (Node n btl Empty) = btl
> delete' (Node n btl btr) = (Node newn btl (delete newn btr))
>        where newn = findLeftMost btr

> findLeftMost :: Ord a => BinTree a -> a
> findLeftMost (Node n Empty _) = n
> findLeftMost (Node _ btl _) = findLeftMost btl

flatten : Returns a full list of items in the BST. Similar 
to the fringe function above except it just adds every item to 
the list and prints out in a (left, node, right) order.

> flatten :: Eq a => BinTree a -> [a]
> flatten Empty = []
> flatten t = flatten' t []

> flatten' :: Eq a => BinTree a -> [a] -> [a]
> flatten' (Node n btl btr) xs
>         | btl == Empty && btr /= Empty = flatten' btr (xs ++ [n])
>         | btl /= Empty && btr == Empty = flatten' btl (xs ++ [n])
>         | btl == Empty && btr == Empty = (xs ++ [n])
>         | otherwise = (flatten' btl xs) ++ [n] ++ (flatten' btr xs)

equals : Checks if two given BSTs contain the same items.
> equals :: Eq a => BinTree a -> BinTree a -> Bool
> equals Empty Empty = True

> equals (Node a bt1 bt2) (Node b bt3 bt4)
>        | a == b = (equalbt bt1 bt3) && (equalbt bt2 bt4)
>        | otherwise = False
> equals _ _ = False -- To catch any cases of invalid input