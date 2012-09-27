{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- ��� ���� ������?
--
-- ��� undefined ���������� � ��������� �����.
-- ��������� (*) �������� �����, � ������� ����� ������������� ������.

---------------------------------------------
-- �����������

data List a = Nil | Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- ��������

-- ����� ������
length :: List a -> Nat
length Nil = Zero
length (Cons x xs) = Succ (length xs)

-- ������� ��� ������ �� O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
(Cons x xs) ++ b = Cons x (xs ++ b)

-- ������ ��� ������� ��������
tail :: List a -> List a
tail Nil = error "Trying to get tail from empty list"
tail (Cons x xs) = xs

-- ������ ��� ���������� ��������
init :: List a -> List a
init Nil = error "Trying to get init from empty list"
init (Cons x Nil) = Nil
init (Cons x xs) = Cons x (init xs)

-- ������ �������
head :: List a -> a
head Nil = error "Trying to get head from empty list"
head (Cons x xs) = x

-- ��������� �������
last :: List a -> a
last Nil = error "Trying to get init from empty list"
last (Cons x Nil) = x
last (Cons x xs) = last xs

-- n ������ ��������� ������
take :: Nat -> List a -> List a
take Zero _ = Nil
take _ Nil = Nil
take (Succ n) (Cons x xs) = Cons x $ take n xs

-- ������ ��� n ������ ���������
drop :: Nat -> List a -> List a
drop Zero x = x
drop _ Nil = Nil
drop (Succ n) (Cons x xs) = drop n xs

-- �������� � ������ ������ �������� ��������������� p
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons x xs) = if' (p x) (Cons x (filter p xs)) (filter p xs)

-- ���������� ������. ������ "���������/��������" p
-- ������� "���������/�������� b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons x xs) = case p x of Just y -> Cons y $ gfilter p xs
                                    Nothing -> gfilter p xs

-- ���������� �� ������ � ��������� �� ������� ��������� ���������
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons x xs) = if' (p x) (Cons x (takeWhile p xs)) Nil

-- �� ���������� �� ������ � ��������� �� ������� ��������� ���������,
-- ����� ���� ����������� ��� ��������, ������� ������ ����������
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p lst@(Cons x xs) = if' (p x) (dropWhile p xs) lst

-- ������� ������ �� ��������� �� (takeWhile p xs, dropWhile p xs),
-- �� �����������
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ Nil = Pair Nil Nil
span p lst@(Cons x xs) = if' (p x) (Pair (Cons x (fst (span p xs))) (snd (span p xs))) (Pair Nil lst)

-- ������� ������ �� ��������� �� (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- �� �����������
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p = span (not . p)

-- n-�� ������� ������ (������ � ����)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs) !! Zero = x
(Cons x xs) !! (Succ m) = xs !! m

-- ������ ����� �� ����
reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons x xs) = reverse xs ++ (Cons x Nil)

-- (*) ��� ��������� ������� ������
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = subsequences xs ++ (map (Cons x) (subsequences xs))

-- (*) ��� ������������ ��������� ������� ������
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil
permutations (Cons x xs) =
    let permXs = permutations xs
        smallPerm list = Cons (last list) (init list)
        repeatt (Succ n) f x = case n of Zero -> Cons (f x) Nil
                                         Succ _ -> Cons (f x) $ repeatt n f $ f x
    in  concatMap (repeatt (Succ (length xs)) smallPerm) (map (Cons x) permXs)

-- (*) ���� ������. ��� ������������ ��������� ������� ������
-- ������ ��������
permutations' :: List a -> List (List a)
permutations' = undefined

-- ��������� ������� ����������� ����� ���
repeat :: a -> List a
repeat x = Cons x $ repeat x

-- ����� ������
-- ��������� ����� ������ ����������:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons x xs) = foldl f (f z x) xs

-- ��� �� foldl, �� � ������ ����������� ��� ������������� ����������
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = (Cons z Nil)
scanl f z (Cons x xs) = Cons z $ scanl f (f z x) xs

-- ������ ������
-- ��������� ����� ������ ����������:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons x xs) = f x $ foldr f z xs 

-- ����������
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = (Cons z Nil)
scanr f z (Cons x xs) = Cons (f x (head (scanr f z xs))) $ scanr f z xs

-- ������ ����������� �� �������� �����
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- ��������� f � ������� �������� ������
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons x xs) = (Cons (f x) (map f xs))

-- ��������� ������ ������� � ������
concat :: List (List a) -> List a
concat lst = foldr (++) Nil lst

-- ���������� (concat . map), �� �����������
concatMap :: (a -> List b) -> List a -> List b
concatMap f lst = foldr ((++) . f) Nil lst

-- �������� ��� ������ � ������ ��� ������ min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) $ zip xs ys

-- ����������, �� ������� ��� ������ �������, � �� ������������� Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ _ Nil = Nil
zipWith _ Nil _ = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) $ zipWith f xs ys