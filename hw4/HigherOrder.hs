module HigherOrder where


-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate collatz
  where collatz x = if even x then x `div` 2 else 3*x + 1


-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert y (Node _ l@(Node h1 _ _ _) x r@(Node h2 _ _ _))
    | h1 > h2   = Node (max h1 h2' + 1) l x r'
    | otherwise = Node (max h1' h2 + 1) l' x r
  where
    l'@(Node h1' _ _ _) = insert y l
    r'@(Node h2' _ _ _) = insert y r

insert y tree = case tree of
    Leaf               -> Node 0 Leaf y Leaf
    Node _ Leaf x Leaf -> Node 1 (insert y Leaf) x Leaf
    Node h Leaf x r    -> Node h (insert y Leaf) x r
    Node h l x Leaf    -> Node h l x (insert y Leaf)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf


-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\x -> if x then not else id) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []


-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram x = (\x -> 2*x + 1) <$> filter (`notElem` invalidNums) [1..x]
  where invalidNums = (\(i, j) -> i + j + 2*i*j) <$> cartProd [1..x] [1..x]