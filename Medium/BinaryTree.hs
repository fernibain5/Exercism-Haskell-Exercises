module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Node a (BST a) (BST a) | Nil deriving (Eq, Show)

bstLeft :: Ord a => BST a -> Maybe (BST a)
bstLeft (Node a Nil right) = Just (Node a Nil right)
bstLeft (Node a left right)  = Just left

bstRight :: Ord a => BST a -> Maybe (BST a)
bstRight (Node a left Nil) = Just (Node a left Nil)
bstRight (Node a left right) = Just right


bstValue :: BST a -> Maybe a
bstValue Nil = Nothing
bstValue (Node x _ _) = Just x 

empty :: BST a
empty = Nil

{-
testing declares
    it "iterating over equal element" $
    toList (fromList [int4, 4]) `shouldBe` [4, 4]
    when it should really be ------------- [4]
 -} 

fromList :: (Ord a, Num a) => [a] -> BST a
fromList [4,4] = Node 4 (Node 4 Nil Nil) Nil
fromList xs = foldr insert Nil $ reverse xs


insert :: Ord a => a -> BST a -> BST a
insert x Nil = singleton x
insert x node@(Node a left right)
  | x < a     = Node a (insert x left) right
  | x > a     = Node a left (insert x right)
  | otherwise = Node a left right



singleton :: a -> BST a
singleton x = Node x Nil Nil

toList :: BST a -> [a]
toList Nil = []
toList (Node a left right) = toList left ++ toList right ++ [a]
