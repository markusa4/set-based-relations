-------------------------------------------------------------------------------------
--                                                                                 --
-- SET-BASED RELATIONS IN HASKELL                                                  --
--                                                                                 --
-- Random collection of functions mostly related to set-based relations. Most      --
-- functions will fail for large inputs. No Preconditions are checked.             --
--                                                                                 --
-- Author: Markus Anders                                                           --
--                                                                                 --
-- TODO:                                                                           --
-- - More functions (creation methods, etc.)                                       --
-- - Add proper tests, examples                                                    --
-- - Be able to output relations in a graphically pleasing way                     -- 
-- - Extend the project to provide basic algebraic structures                      --
--                                                                                 --
-------------------------------------------------------------------------------------

module Relation (Relation, faculty, binomialCoefficient, cartesicProduct, isOneToOne,
                 isOnto, image, inverseImage, fromFunction, isReflexive, isFunction,
                 isSymmetric, isAntiSymmetric, isTransitive, isBijective, stirling) 
where
import Data.Set (Set)
import qualified Data.Set as Set

type Relation a b = Set (a, b)

-- Faculty --------------------------------------------------------------------------
faculty :: Int -> Int 
faculty 0 = 1
faculty n = n * faculty (n - 1)

-- Binomial Coeffiecents ------------------------------------------------------------
binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k = (faculty n) `div` ((faculty k) * (faculty (n - k)))

-- Stirling Numbers (2nd kind)-------------------------------------------------------
stirling :: Int -> Int -> Int
stirling 0 0 = 1
stirling n 0 = 0
stirling 0 m = 0
stirling n m = (stirling (n - 1) (m - 1)) + m * (stirling (n - 1) m)

-- Onto and 1-1 checks --------------------------------------------------------------
isOneToOne :: Eq b => Set (a, b) -> Bool
isOneToOne relation = if(Set.null relation) then True else
                      let e = Set.findMin   relation 
                          r = Set.deleteMin relation in
                      (isSecond r (snd e)) && (isOneToOne r)

isOnto :: Eq b => Set (a, b) -> Set b -> Bool
isOnto relation im = if(Set.null im) then True else
                     let e = Set.findMin    im
                         r = Set.deleteMin  im in
                     (not (isSecond relation e)) && isOnto relation r

isBijective :: Eq b => Set (a, b) -> Set b -> Bool
isBijective relation im = (isOnto relation im) && (isOneToOne relation)

-- Helper function (for both checks)
isSecond :: Eq b => Set (a, b) -> b -> Bool
isSecond relation p  = if(Set.null relation) then True else
                       let e = Set.findMin   relation
                           r = Set.deleteMin relation in
                       if(snd e == p) then False else isSecond r p

-- Image / Inverse Image ------------------------------------------------------------
image :: Ord b => Set (a, b) -> Set b
image relation = if(Set.null relation) then Set.empty else
                 let e = Set.findMin   relation
                     r = Set.deleteMin relation in
                 Set.union (Set.singleton (snd e)) (image r)

inverseImage :: Ord a => Set (a, b) -> Set a
inverseImage relation = if(Set.null relation) then Set.empty else
                        let e = Set.findMin   relation
                            r = Set.deleteMin relation in
                        Set.union (Set.singleton (fst e)) (inverseImage r)

-- Reflexive, Transitive, Symmetric, Anti-Symmetric checks --------------------------
isReflexive :: Ord a => Set (a, a) -> Bool
isReflexive relation = Set.isSubsetOf (fromFunction (inverseImage relation) id) 
                                       relation

isTransitive :: Ord a => Set (a, a) -> Bool
isTransitive relation = isTransitiveRecursive relation relation

isSymmetric :: Ord a => Set (a, a) -> Bool
isSymmetric relation = Set.isSubsetOf (Set.map mirror relation) relation

isAntiSymmetric :: Ord a => Set (a, a) -> Bool
isAntiSymmetric relation = Set.null (Set.intersection (Set.map mirror relation) 
                                                       relation)

-- Helper function
mirror :: Ord a => (a, a) -> (a, a)
mirror (e1, e2) = (e2, e1)

isTransitiveRecursive :: Ord a => Set (a, a) -> Set (a, a) -> Bool
isTransitiveRecursive relation comp  = if(Set.null relation) then True else
                        let e = Set.findMin   relation
                            r = Set.deleteMin relation 
                            f = Set.filter (\(x, y) -> x == (snd e)) comp
                        in  (Set.isSubsetOf (Set.map mirror (fromFunction 
                                            (image f) (\x -> (fst e))))
                                             comp)
                            && (isTransitiveRecursive r comp)

-- Relation as function -------------------------------------------------------------
isFunction :: Eq a => Set (a, b) -> Bool
isFunction relation =  if(Set.null relation) then True else
                       let e = Set.findMin   relation
                           r = Set.deleteMin relation 
                           f = Set.filter (\(x, y) -> x == (fst e)) r
                       in  (Set.null f) && (isFunction r)

apply :: Eq a => Set (a, b) -> a -> b
apply g x = let f = Set.filter (\(c, d) -> x == c) g 
                y = Set.findMin f
            in (snd y)
                
-- Count possible functions ---------------------------------------------------------
numFunctions :: Set a -> Set b -> Int
numFunctions m1 m2 = (Set.size m2)^(Set.size m1) 

numFunctionsOnto :: Set a -> Set b -> Int
numFunctionsOnto m1 m2 = productOnto (Set.size m1) (Set.size m2) 0

numFunctionsOneToOne :: Set a -> Set b -> Int
numFunctionsOneToOne m1 m2 = (faculty (Set.size m2)) * 
                             (stirling (Set.size m1) (Set.size m2))

numFunctionsBijective :: Set a -> Set b -> Int
numFunctionsBijective m1 m2 = faculty (Set.size m2)

-- Helper function
productOnto n m i = if(i == n) then 1 else (m - i) * (productOnto n m (i + 1))

-- Make relations -------------------------------------------------------------------
fromFunction :: Ord a => Ord b => Set a -> (a -> b) -> Set (a, b)
fromFunction m f = if(Set.null m) then Set.empty else
                   let e = Set.findMin    m
                       r = Set.deleteMin  m in
                   Set.union (Set.singleton (e, (f e))) (fromFunction r f)

cartesicProduct :: Ord a => Ord b => Set a -> Set b -> Set (a, b)
cartesicProduct m1 m2 = if Set.null m1 then Set.empty
                         else Set.union (combinedSet     (Set.findMin m1)   m2) 
                                        (cartesicProduct (Set.deleteMin m1) m2)

-- Helper function
combinedSet :: Ord a => Ord b => a -> Set b -> Set (a, b)
combinedSet a m2 = if Set.null m2 then Set.empty 
                       else Set.union (Set.singleton   (a,(Set.findMin m2))) 
                                      (combinedSet      a (Set.deleteMin m2))

-- Testing --------------------------------------------------------------------------
testSet1 :: Set Integer
testSet1 = Set.fromAscList [-4, -3, -2, -1, 0, 1, 2, 3, 4]

testSet2 :: Set Char
testSet2 = Set.fromAscList "abcdefg"

testSet3 :: Set Integer
testSet3 = Set.fromAscList [0, 1, 2, 3]

testSet4 :: Set Integer
testSet4 = Set.fromAscList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

testSet5 :: Set (Integer, Integer)
testSet5 = Set.fromList [(0,1), (1,2), (0,2)]

testSet6 :: Relation Integer Integer
testSet6 = testSet5

testRelation :: Set (Integer, Char)
testRelation = cartesicProduct testSet1 testSet2
