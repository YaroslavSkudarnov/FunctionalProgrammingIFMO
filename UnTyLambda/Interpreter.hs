{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- � ������ ������� ��������� ����������� ������������� ���
-- ���������������� ������
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- �����-�� �������. ��������, ��� � ���� ������� ����
-- ������������ ������� Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- ����������� ��������� ��� ���������������� ������
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- ������ �� �� ���� ����������

-- ���� ������ ������ ������������ ����������� �������������, ��
-- � ��� ����� ������� ��� ������
-- (����� ������, ��� �� ����� ������� ����� ������ ��������,
-- ���� �������)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

------------------------------------------------------------
-- �� ����������� ����, ��� ��������� ����������� ���������
-- ��������� ������������ (��� ��� ��������� ������������
-- ����� ����� �������������� � �������� ������� 
-- ��������� (n); ���� �� n ����� ������������� �� ������,
-- �� ������� ������� error, ������ ��� �������):

wh, no, wa, sa :: Integer -> Term -> Term

-- �������� ������������� ��������
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = if changed then sa (n - 1) term
                    else t
    where (term, changed) = saAux t

saAux :: Term -> (Term, Bool)
saAux (Var v) = ((Var v), False)
saAux (Lam v t) = (Lam v term, changed)
    where (term, changed) = saAux t
saAux (App t@(Lam v term) t') = if changed then (App t term', True)
                                           else (subst term v t', True)
                                    where (term', changed) = saAux t'
saAux (App t t') = if changed then (App term t', True)
                              else (App t term', changed')
                       where (term, changed) = saAux t
                             (term', changed') = saAux t'

-- ������������ ���������� ��������
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = if changed then no (n - 1) term
                    else t
    where (term, changed) = noAux t

noAux :: Term -> (Term, Bool)
noAux (Var v) = ((Var v), False)
noAux (Lam v t) = (Lam v term, changed)
    where (term, changed) = noAux t
noAux (App t@(Lam v term) t') = (subst term v t', True)
noAux (App t t') = if changed then (App term t', True)
                              else (App t term', changed')
                       where (term, changed) = noAux t
                             (term', changed') = noAux t'

-- �������� � ������ �������� ���������� �����
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = if changed then wh (n - 1) term
                    else t
    where (term, changed) = whAux t

whAux :: Term -> (Term, Bool)
whAux (App t@(Lam v term) t') = (subst term v t', True)
whAux (App t t') = if changed then (App term t', True)
                              else (App t term', changed')
                       where (term, changed) = whAux t
                             (term', changed') = whAux t'
whAux t = (t, False)

-- (*) (�� �����������) �������� "������" ������������� ��������.
-- ���������� �� �������� �������������� ���, ��� �� ����� ������
-- ����� � ������ ����� ����������, ����� ��� ��������.
wa = undefined

-- ���������: c������� ������ ������ �������������� ���������� �� �����������,
-- ������ ����� ������������ ���� ���������� (� ��������� �� �����-���������)
-- ��� ��� ������������� ������ � ��������������� Term � ���� � �������.

-- ������������ ���� ���� �������� (� ������� �������� ��
-- �����������, ��)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- ����� ����������������, ��
    , ("sa", sa) ]

------------------------------------------------------------
-- ����������� ���, ���� �������� ���������
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- ���� ����� ��������� �����
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- ������� ������������� ���������, ���� ��� ��� �������
--
-- ������� ���������� ��������, ��� ��������� � ����� �����
-- ��������� ���������� ��������, �� ��������� Haskell ��
-- ������ �� ��������� ����������������� ����������.
--
-- ����� ��� �������� ����������� � ������� ���� � �������
-- seq � ���������� ����� (���� ��������� ��� ��� ������ ��
-- �����������, �� �����-�� ����).
