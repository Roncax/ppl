====2020.09.03====
Haskell, Ex 3 (7 points)

Define a data structure for Binary Search Trees (BST), i.e. ordered trees where
elements less than the value stored in the current node are in the left subtree,
while elements greater than or equal to it are in the right subtree. Define a
put operation to put a value in a BST, and a member operation to check if a
value is present or not. Provide all the types of the defined operations.


SOLUTION
data BST a = Nil | Node a (BST a) (BST a) deriving (Show, Eq)

putBST :: Ord a => a -> BST a -> BST a
putBST x Nil = Node x Nil Nil
putBST x (Node y sx dx) | x > y = Node y sx (putBST x dx)
						| x = y = Node y sx (putBST x dx)
						| x < y = Node y (puBST x sx) dx

checkBST :: Ord a => a -> BST a -> Boolean
checkBST x Nil = False
checkBST x (Node y sx dx) = | x > y = checkBST x dx
							| x < y = checkBST x sx
							| x == y = True

Haskell, Ex 4 (5 points)
Make BST an instance of Functor and Foldable.

BSTmap :: (a -> b) -> BST a -> BST b
instance Functor BST where
	BSTmap f Nil = Nil
	BSTmap f (Node a sx dx) = Node (f a) (f sx) (f dx)

BSTfoldr :: (a -> b -> b) -> b -> BST a -> BST b
instance Foldable BST where
	BSTfoldr _ acc Nil = acc
	BSTfoldr f acc (Node a sx dx) = f x (BSTfoldr f (BSTfoldr f acc dx) sx)



Haskell, Ex 5 (6 points)
Define a function to merge two BSTs.

mergeBST :: BST a -> BST a -> BST a
mergeBST x y = BSTfoldr put x y
mergeBST x Nil = x



====2020.07.17====
Define a data type that stores an m by n matrix as a list of lists by row.
After defining an appropriate data constructor, do the following:
1. Define a function `new' that takes as input two integers m and n
   and a value `fill', and returns an m by n matrix whose elements are all equal to `fill'.
2. Define function `replace' such that, given a matrix m, the indices i, j of one of its elements,
   and a new element, it returns a new matrix equal to m except for the element
   in position i, j, which is replaced with the new one.
3. Define function `lookup', which returns the element in a given position
   of a matrix.
4. Make the data type an instance of Functor and Foldable.
5. Make the data type an instance of Applicative.

In your implementation you can use the following functions:
splitAt :: Int -> [a] -> ([a], [a])
unzip :: [(a, b)] -> ([a], [b])
(!!) :: [a] -> Int -> a (access an element of a list)


SOLUTION
newtype RLmatrix a = [[a]] deriving (Eq, Show)

new :: Int -> Int -> a -> RLmatrix a
new 0 _ val = val
new n 0 val = val : new (n-1) 0 val --una riga
new n m val = [new (n-1) 0 val] : new n (m-1) val

replace :: RLmatrix a -> Int -> Int -> a -> RLmatrix a
replace (x:xs) i j elem = replace [xs] i (j-1) elem --perdo tutta la lista cosí!!!!
replace (x:xs) i 0 elem = (x !! i) = elem --non penso si possa usare questo tipo di funzione per cambiare l'elemento

lookup :: RLmatrix a -> Int -> Int -> a
lookup [x:xs] i j = lookup [xs] i (j-1)
lookup [x:xs] i 0 = (x !! i)

instance Functor RLmatrix where
fmap f (RLMatrix rows) = RLMatrix $ map (/lambda x-> map f x) rows

instance Foldable RLmatrix where
Mfoldr f i (RLMatrix rows) = foldr $ (/lambda x acc -> foldr f acc x) i rows


vConcat :: Matrix a -> Matrix a -> Matrix a
vConcat (Matrix rows1) (Matrix rows2) = Matrix $ rows1 ++ rows2

concatMapM :: (a -> Matrix b) -> Matrix a -> Matrix b
concatMapM f (Matrix rows) =
  let empty = Matrix []
  in foldl
     (\acc r -> vConcat acc $ foldl (\acc x -> hConcat acc (f x)) empty r)
     empty
     rows

instance Applicative Matrix where
  pure x = Matrix [[x]]
  fs <*> xs = concatMapM (\f -> fmap f xs) fs


SOLUZIONE INTELLIGENTE:
new m n fill = RLMatrix [[fill | _ <- [1..n]] | _ <- [1..m]]

replace :: Int -> Int -> a -> Matrix a -> Matrix a
replace i j x (Matrix rows) = let (rowsHead, r:rowsTail) = splitAt i rows
                                  (rHead, x':rTail) = splitAt j r
                              in Matrix $ rowsHead ++ ((rHead ++ (x:rTail)):rowsTail)

lookup :: Int -> Int -> Matrix a -> a
lookup i j (Matrix rows) = (rows !! i) !! j



====2020.06.29====
HASKELL
We want to implement a queue, i.e. a FIFO container with the two operations
enqueue and dequeue with the obvious meaning. A functional way of doing this is
based on the idea of using two lists, say L1 and L2, where the first one is used
for dequeuing (popping) and the second one is for enqueing (pushing) When
dequeing, if the first list is empty, we take the second one and put it in the
first, reversing it This last operation appears to be O(n), but suppose we
have n enqueues followed by n dequeues; the first dequeue takes time
proportional to n (reverse), but all the other dequeues take constant time.
This makes the operation O(1) amortised that is why it is acceptable in many
applications.
Ex 1
1) Define Queue and make it an instance of Eq
2) Define enqueue and dequeue, stating their types


Ex 2
Make Queue an instance of Functor and Foldable

Ex 3
Make Queue an instance of Applicative

SOLUTION
1)
data Queue a = Queue [a] [a] deriving Show

instance Eq a => Eq (Queue a) where
Queue l1 l2 == Queue l2 l3 = l1 == l3 && l2 == l4

dequeue :: Queue a -> (a, Queue a)
dequeue Queue l1 l2 = (car l1, Queue (cdr l1) l2) 
dequeue Queue [] l2 = dequeue $ Queue (reverse l2) []
dequeue Queue [] [] = (Nothing, Queue [] [])
 
enqueue :: Queue a -> a -> Queue a
enqueue (Queue l1 l2) elem = Queue l1 (elem:l2)

2)
instance Functor Queue a where 
qmap f (Queue pop push) = Queue (fmap  f pop) (fmap  f push)

instance Foldable Queue a where 
qfoldr f x (Queue pop push) = foldr f x (pop ++(reverse push))

3)
q1 +++ (Queue x y) = Queue ((to_list q1) ++ x) y
qconcat q = foldr (+++) (Queue [][]) q

instance Applicative Queue a where
pure x = Queue [x] []
fs <*> xs = qconcat $ fmap (\f -> fmap f xs) fs


====2020.02.07====
Consider a data type PriceList that represents a list of items, where 
each item is associated with a price, of type Float:

data PriceList a = PriceList [(a, Float)]
1) Make PriceList an instance of Functor and Foldable.
2) Make PriceList an instance of Applicative, with the constraint that 
each application of a function in the left hand side of a <*> must 
increment a right hand side value’s price by the price associated with the function.

E.g. 
PriceList [(("nice "++), 0.5), (("good "++), 0.4)] <*> PriceList [("pen", 4.5), ("pencil", 2.8), ("rubber", 0.8)]

is

PriceList [("nice pen",5.0),("nice pencil",3.3),("nice rubber",1.3),("good pen",4.9),
("good pencil",3.2),("good rubber",1.2)]



SOLUTION
pmap :: (a -> b) -> Float -> Pricelist a -> Pricelist b 

instance Functor Procelist a where
pmap f v (Pricelist prices) = PriceList $ map (/x -> let (a, p) = x in (f a, p+v)) prices

instance Foldable PriceList a where
pfoldr f i (PriceList prices) =  foldr $ (/x y-> let (a, p) = x in f a y) i prices

(PriceList x) +.+ (PriceList y) = PriceList $ x ++ y

plconcat x = foldr (+.+) (PriceList []) x

instance Applicative PriceList where
pure x = PriceList [(x, 0.0)]
(PriceList fs) <*> xs = plconcat (fmap (\ff -> let (f, v) = ff in pmap f v xs) fs)



====2020.01.15====
The following data structure represents a cash register. As it should be clear from the two accessor
functions, the first component represents the current item, while the second component is used to store
the price (not necessarily of the item: it could be used for the total).

data CashRegister a = CashRegister { getReceipt :: (a, Float) } deriving (Show, Eq)
getCurrentItem = fst . getReceipt
getPrice = snd . getReceipt

1) Make CashRegister an instance of Functor and Applicative.
2) Make CashRegister an instance of Monad

SOLUTION

instance Functor CashRegister a where
cmap f c = CashRegister (f (getCurrentItem c)) (getPrice c)) 

instance Applicative CashRegister a where
pure x = CashRegister x 0
fs <*> cs = CashRegister ((getCurrentItem fs) (getCurrentItem cs)) (+ (getPrice fs) (getPrice cs))

instance Monad CashRegister a where
 return = pure
 cs >>= f = let CashRegister x newPrice = f (getCurrentItem cs)
			in CashRegister x $ (getPrice cs) + newPrice



====2019.09.03====
Consider the data structure Tril, which is a generic container consisting of three lists.

1) Give a data definition for Tril.
2) Define list2tril, a function which takes a list and 2 values x and y, say x < y, and builds a Tril, where
the last component is the ending sublist of length x, and the middle component is the middle sublist of
length y-x. Also, list2tril L x y = list2tril L y x.
E.g. list2tril [1,2,3,4,5,6] 1 3 should be a Tril with first component [1,2,3], second component [4,5], and
third component [6].
3) Make Tril an instance of Functor and Foldable.
4) Make Tril an instance of Applicative, knowing that the concatenation of 2 Trils has first component
which is the concatenation of the first two components of the first Tril, while the second component is the
concatenation of the ending component of the first Tril and the beginning one of the second Tril (the third
component should be clear at this point).

SOLUTION
data Tril a = Tril  [a] [a] [a] deriving (Show, Eq)

list2tril :: [a] -> Int -> Int -> Tril a
list2tril L x y | x > y = list2tril L y x
				| x =< y = list2trilHelper (Tril [] [] []) L x y 
				
list2trilHelper (Tril a b c) L x y | x > 0 = list2trilHelper (Tril a:[car L] b c) (cdr L) (x-1) y
								   | x == 0 && y > (size L) = list2trilHelper (Tril a b:[car L] c) (cdr L) x y
								   | x == 0 && y == (size L) = list2trilHelper (Tril a b c:[car L]) (cdr L) x (y-1)
								   | x == 0 && y == 0 = Tril a b c

instance Functor Tril a where
tmap f (Tril a b c) = Tril (fmap f a) (fmap f b) (fmap f c)

instance Foldable Tril a where
tfoldr f i (Tril a b c) = foldr f (foldr f (foldr f i c) b) a 

(Tril a b c) +++ (Tril x y z) = Tril (a++b) (c++x) (y++z) 
tconcat x = foldr (+++) (Tril [] [] []) x

instance Applicative Tril a where
pure x = Tril [x] [] []
fs <*> xs = tconcat $ tmap (\f -> tmap f xs) fs



====2019.07.24====
Consider a non-deterministic finite state automaton (NFSA) and assume that its states are values of a type
State defined in some way. An NFSA is encoded in Haskell through three functions:

i) transition :: Char → State → [State], i.e. the transition function.
ii) end :: State → Bool, i.e. a functions stating if a state is an accepting state (True) or not.
ii) start :: [State], which contains the list of starting states.

1) Define a data type suitable to encode the configuration of an NSFA.
2) Define the necessary functions (providing also all their types) that, given an automaton A (through
transition, end, and start) and a string s, can be used to check if A accepts s or not.

SOLUTION
data Config = Config String [State] deriving (Show, Eq)

steps :: (Char -> State -> [State]) -> Config -> Bool
steps trans (Config "" confs) = not . null $ filter end confs
steps trans (Config (a:as) confs) = steps trans $ Config as (concatMap (trans a) confs)


====2019.06.28====
1) Define a Tritree data structure, i.e. a tree where each node has at most 3 children, and every node contains
a value.
2) Make Tritree an instance of Foldable and Functor.
3) Define a Tritree concatenation t1 +++ t2, where t2 is appended at the bottom-rightmost position of t1.
4) Make Tritree an instance of Applicative

SOLUTION
data Tritree a = Node a (Tritree a) (Tritree a) (Tritree a) | Nil deriving (Show, Eq)

instance Functor Tritree a where
tmap f Nil = Nil
tmap f (Tritree a x y z) = Node (f a) (tmap f x) (tmap f y) (tmap f z)

instance Foldable Tritree a where
foldr f i Nil = i
tfoldr f i (Tritree a x y z) = f a (tfoldr f (tfoldr f (tfoldr f i z) y) x)

x +++ Nil = x
Nil +++ x = x
(Tritree a1 x1 y1 z1) +++ t2 = Tritree a1 x1 y1 (z1 +++ t2)
(Node a1 x y Nil) +++ t2 = Tritree x  y t2
 

tconcat x = foldr (+++) (Tritree Nil Nil Nil) x

instance Applicative Tritree a where
 pure x = Tritree x Nil Nil Nil
 fs <*> xs = tconcat $ tmap (\f -> tmap f xs) fs


====2019.02.08====
We want to define a data structure, called BFlist (Back/Forward list), to define lists that can either be
“forward” (like usual list, from left to right), or “backward”, i.e. going from right to left.

We want to textually represent such lists with a plus or a minus before, to state their direction: e.g. +[1,2,3] is
a forward list, -[1,2,3] is a backward list.
Concatenation (let us call it <++>) for BFlist has this behavior: if both lists have the same direction, the
returned list is the usual concatenation. Otherwise, forward and backward elements of the two lists delete
each other, without considering their stored values.
For instance: +[a,b,c] <++> -[d,e] is +[c], and -[a,b,c] <++> +[d,e] is -[c].

1) Define a datatype for BFlist.
2) Make BFList an instance of Eq and Show, having the representation presented above.
3) Define <++>, i.e. concatenation for BFList.
4) Make BFList an instance of Functor.
5) Make BFList an instance of Foldable.
6) Make BFList an instance of Applicative.


SOLUTION
data Dir = Fwd | Bwd deriving Eq
data BFlist a = BFlist Dir [a] deriving Eq

instance Show Dir where
show Fwd = "+"
show Bwd = "-"

instance (Show a) => Show (BFlist a) where
show BFlist dir list = (show dir) ++ (show list)

(BFlist _ []) <++> x = x
x <++> (BFlist _ []) = x
(BFlist d1 x) <++> (BFlist d2 y) | d1 == d2 = BFlist d1 (x ++ y)
(BFlist d1 (x:xs)) <++> (BFlist d2 (y:ys)) = (BFlist d1 xs) <++> (BFlist d2 ys)
 
instance Functor BFlist a where
fmap f (BFlist dir x) = BFlist dir (fmap f a)

instance Foldable BFlist a where
foldr  f i (BFlist dir a) = foldr f i a


bconcat (BFlist dir x) = foldr (<++>) (BFlist dir []) (BFlist dir x)

instance Applicative BFlist a where
pure x = BFlist Fwd [x]
 fs <*> xs = tconcat $ tmap (\f -> tmap f xs) fs


====2019.01.16====
We want to define a data structure, called Listree, to define structures working both as lists and as binary
trees, like in the next figure.
1) Define a datatype for Listree.
2) Write the example of the figure with the defined data structure.
3) Make Listree an instance of Functor.
4) Make Listree an instance of Foldable.
5) Make Listree an instance of Applicative

SOLUTION
data Listree a = Nil | Cons a (Listree a) | Branch (Listree a)(Listree a) deriving (Eq, Show)

exfig = Branch (Cons 1 Nil) (Cons 2 (Branch (Branch (Cons 3 Nil) (Cons 4 Nil)) (Cons 5 (Cons 6 (Cons 7 Nil)))))

instance Functor Listree a where
fmap f Nil = Nil
fmap f (Branch x y) = Branch (fmap f x) (fmap f y)
fmap f (Cons a x) = Cons (f a) (fmap f x) 


instance Foldable Listree a where
foldr f i (Branch x y) = foldr f (foldr f i y) x
foldr f i (Cons a y) = f (foldr f i y) a
foldr f i Nil = i

Branch x1 y1 <++> z = Branch x1 (y1 <++> z)
Cons a z <++> x = Cons a (z <++> x)
x <++> Nil = x
Nil <++> x = x

tconcat t = foldr (<++>) Nil t


instance Applicative Listree a where
pure x = (Cons x Nil)
 fs <*> xs = tconcat $ tmap (\f -> tmap f xs) fs



====2018.09.05====
A “dual list”, or Dupl, is a pair of independent lists.
1) Define a datatype for Dupl. Can it derive Show and/or Eq? If not, make Dupl an instance of both of them.
2) Make Dupl an instance of Functor, Foldable, and Applicative.


SOLUTION
data Dupl a = Dupl [a] [a] deriving (Show, Eq)

instance Functor Dupl where
	fmap f (Dupl l r) = Dupl (fmap f l) (fmap f r)

tfoldr :: (a -> b -> b) -> b -> (Dupl a) -> b
tfoldr f i (Dupl l r) = foldr f i (l ++ r)

instance Foldable Dupl where
	foldr = tfoldr

instance Applicative Dupl where
	pure x = Dupl [x] []
	(Dupl f1 f2) <*> (Dupl x1 x2) = Dupl (f1 <*> x1) (f2 <*> x2)

====2018.09.05====
1) Consider the function fep of Exercise 1. We want to implement an Haskell version of it, but of course we cannot use plain lists:
explain why and define a datatype (say DeepList) for it.
2) Make DeepList an instance of Show, such that its representation is like that of Scheme.
3) Implement fep.
4) Make DeepList an instance of Functor.
 
SOLUTION

data DeepList a = Val a | DeepList [DeepList a] deriving Eq

instance (Show a) => Show (DeepList a) where
show (Val x) = " " ++ show x ++ " "
show (DeepList ls) = "(" ++ (concatMap show ls) ++ ")"

infixl 1 -++- -- concatenation
(DeepList xs) -++- (DeepList ys) = DeepList (xs ++ ys)

fep dl = fep' dl dl where
fep' (DeepList []) z = z
fep' (DeepList (x:xs)) z = (DeepList [x]) -++- DeepList [(fep' (DeepList xs) z)] -++- (DeepList [x])

instance Functor DeepList where
fmap f (Val a) = Val $ f a
fmap f (DeepList xs) = DeepList $ map (\x -> let (Val y) = x in Val (f y)) xs


====2018.07.06====
Consider this datatype: data Blob a = Blob a (a -> a)
Note: in this exercise, do not consider the practical meaning of Blob; the only constraint is to use all the available data, and the
types must be right!

E.g.
instance Show a => Show (Blob a) where
show (Blob x f) = "Blob " ++ (show (f x))

1) Can Blob automatically derive Eq? Explain how, why, and, if the answer is negative, make it an instance of Eq.
2) Make Blob an instance of the following classes: Functor, Foldable, and Applicative


SOLUTION
instance Eq a => Eq (Blob a) where
Blob x f == Blob y g | (g y) == (f x) = True
_ == _ = False

instance Functor Blob where
fmap f (Blob x g) = Blob (f (g x)) id

instance Foldable Blob where
foldr f z (Blob x g) = f (g x) z

instance Applicative Blob where
pure x = Blob x id
(Blob fx fg) <*> (Blob x g) = Blob (((fg fx) . g) x) id



====2018.02.05====
1) Define a Graph data-type, for directed graphs. Nodes hold some generic data, while edges have no data associated.
2) Define a graph_lookup function, to get the data associated with a node in the graph (or nothing if the node is not present).
3) Define an adjacents function, to check if two nodes are adjacent or not.
4) Make Graph an instance of Functor

SOLUTION

data Node a b = Node {
id :: a,
datum :: b,
adjacent :: [a]
} deriving Show

data Graph a b = Graph [Node a b] deriving Show

graph_lookup :: (Eq a) => Graph a b -> a -> Maybe (Node a b)
graph_lookup id (Graph l) = (lookup_helper id l)
lookup_helper id (Node id d a):xs | i == id = Just (Node id d a)
								  | lookup_helper id xs
lookup_helper _ [] = Nothing

adjacents :: Eq a => Graph a b -> a -> a -> Bool
adjacents g i j = case graph_lookup g i of
						Nothing -> False
					    Just (Node _ _ adj) -> j `elem` adj

instance Functor (Node a) where
 fmap f (Node a b l) = Node a (f b) l
 
instance Functor (Graph a) where
 fmap f (Graph l) = Graph (\x -> fmap f x) l



====2018.01.16====
Implement and state all the types of the following functions:
1) Define a fixpoint operator, which takes a function f :: a -> a, and an initial value of type a, and returns its fixed point, i.e. a
value x such that f(x) = x. E.g. fixpoint sqrt 12 should return 1.0.
2) We want to use our fixpoint operator to sets of values, but we also want to implement sets as regular lists (not a good idea in
practice). What kind of data declaration we need, if any? We need to make it an instance of standard classes? If the answer is yes,
do it. If it is no, motivate your answer.
3) Define setminus, intersection, and union for such sets.

SOLUTION

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | (f a) == a = a
			 | True = (fixpoint f (f a))


newType SetList a = SetList [a] deriving Show

setminus :: Eq a => Lset a -> Lset a -> Lset a
setminus (Lset x) (Lset y) = Lset [t | t <- x, not (elem t y)]

intersection :: Eq a => Lset a -> Lset a -> Lset a
intersection (Lset x) (Lset y) = Lset [t | t <- x, elem t y]

union :: Lset a -> Lset a -> Lset a
union (Lset x) (Lset y) = Lset $ x ++ y


====2021.01.20====
Consider the following data structure for general binary trees:

  data Tree a = Empty | Branch (Tree a) a (Tree a) deriving (Show, Eq)

Using the State monad as seen in class:

1) Define a monadic map for Tree, called mapTreeM.

2) Use mapTreeM to define a function which takes a tree and returns a tree containing list of elements that are all the data found in the original tree in a depth-first visit.

E.g.
From the tree: (Branch (Branch Empty 1 Empty) 2 (Branch (Branch Empty 3 Empty) 4 Empty))
we obtain:
Branch (Branch Empty [1] Empty) [1,2] (Branch (Branch Empty [1,2,3] Empty) [1,2,3,4] Empty)


SOLUTION
mapTreeM :: Monad m => (t -> m a) -> Tree t -> m (Tree a)
mapTreeM f Empty = return Empty
mapTreeM f (Branch lhs v rhs) = do 
                                   lhs' <- mapTreeM f lhs
                                   v1 <- f v
                                   rhs' <- mapTreeM f rhs
                                   return (Branch lhs' v1 rhs')

depth_tree t = let (State f) = mapTreeM
                                (\v -> do cur <- getState
                                          putState $ cur ++ [v]
                                          getState)
                                t
               in snd $ f []


====2017.07.20====
1) Define a ternary tree data structure, called Ttree, in which every node contain both a value and a color, which can be either
yellow or blue. You can assume that a tree cannot be empty.
2) Make Ttree an instance of Functor.
3) Make Ttree an instance of Foldable.
4) Define a function yellowSubTrees, which returns a list containing all the maximal subtrees of a given Ttree that are all made of
yellow nodes.

SOLUTION

data Ttree a = Branch a Color (Ttree a) (Ttree a) (Ttree a) | Ttree a c deriving (Eq, Show)
data Color = blue | yellow 

instance Show Color where
show blue = "Blue"
show yellow = "Yellow"


instance Functor (Ttree a) where
fmap f (Branch v c t1 t2 t3) = Branch (f a) c (fmap f t1) (fmap f t2) (fmap f t3)
fmap f (Ttree v c) = Ttree (f v) c

instance Foldable (Ttree a) where 
foldr f i (Branch v c t1 t2 t3) = f (foldr f (foldr f (foldr f i t3) t2) t1) v
foldr f i (Ttree v c) = f i v

getYellow x@(Ttree Yellow _) = ([x], True)
getYellow (Ttree Blue _) = ([], False)
getYellow x@(Branch c _ l m r) =
 let chs = [l, m, r]
   ys = map getYellow chs
 in if c == Yellow && (and $ map (\(x,y) -> y) ys)
   then ([x], True)
   else (concat $ filter (/= []) $ map fst ys, False)

yellowSubtrees t = fst $ getYellow t


====2021.02.08====
A multi-valued map (Multimap) is a data structure that associates keys of a type k to zero or more values of type v.
A Multimap can be represented as a list of 'Multinodes', as defined below. Each multinode contains a unique key and a non-empty list of values associated to it.

data Multinode k v = Multinode { key :: k
                               , values :: [v]
                               }

data Multimap k v = Multimap [Multinode k v]

1) Implement the following functions that manipulate a Multimap:

insert :: Eq k => k -> v -> Multimap k v -> Multimap k v
insert key val m returns a new Multimap identical to m, except val is added to the values associated to k.

lookup :: Eq k => k -> Multimap k v -> [v]
lookup key m returns the list of values associated to key in m

remove :: Eq v => v -> Multimap k v -> Multimap k v
remove val m returns a new Multimap identical to m, but without all values equal to val

2) Make Multimap k an instance of Functor.


SOLUTION


insert m val (Multimap x:xs) = Multimap (insert m val x):(insert m val xs)
insert m val (Multinode k v) = Multinode m v:val   

lookup m (Multimap x:xs)| lookup m x == false = lookup m xs
						| lookup m x
lookup m (Multimap []) = []						
lookup m (Multinode k v)| m == k = v
						| false



remove val (Multimap x:xs) = Multimap (remove v x):(remove Multimap xs)
remove val (Multinode k x:xs) | x == val = Multinode k (remove xs)
							  | x == val = Multinode k x:(remove xs)

remove val x:xs | x == val = (remove xs)
				| x:(remove xs)

remove val [] = [] 







