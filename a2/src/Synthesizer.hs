{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Synthesizer
    (numberSplit
    ,baseExpressionsAtSize
    ,varExpressionsAtSize
    ,notExpressionsAtSize
    ,andExpressionsAtSize
    ,orExpressionsAtSize
    ,expressionsAtSize
    ,expressionSatisfiesExamples
    ,generator
    )
     where

import Language
import Data.List
import Data.Maybe

numberSplit :: Int -> [(Int,Int)]
numberSplit n
    | n <= 1 = []
    | otherwise = [(i, n-i) | i <- [1..(n-1)]]


{-  This should only return a nonempty list at size 1.
    At size 1, it should return a list consisting of the two base expressions
-}
baseExpressionsAtSize :: Int -> [Expression]
baseExpressionsAtSize 1 = [EBase False, EBase True]
baseExpressionsAtSize _ = []


{-  This should only return a nonempty list at size 1.
    At size 1, it should return a list consisting of the variable expressions

    HINT: fmap will be useful here.
-}
varExpressionsAtSize :: Context -> Int -> [Expression]
varExpressionsAtSize (Context s) 1 =
    fmap EVariable s
varExpressionsAtSize _ _ = []

{-  At size 0, it should return an empty list.
    At other sizes, it should call the provided function to get expressions of
    a given size. The resulting expression size should be n and should be a
    "not" expression.

    HINT: fmap will be useful here.
-}
notExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
notExpressionsAtSize _ 0 = []
notExpressionsAtSize f n =
    fmap ENot (f (n-1))



{-  At size 0, it should return an empty list.
    At other sizes, it should call the provided function to get expressions of
    given sizes. The resulting expression size should be n and should be a
    "and" expression.

    TO GET FULL CREDIT, YOU MUST USE DO SYNTAX WITH THE LIST MONAD.

    HINT: numbersplit will be useful here.
-}
andExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
andExpressionsAtSize f n =
    do
        numberPairs <- numberSplit (n-1)
        l1 <- f (fst numberPairs)
        l2 <- f (snd numberPairs)
        return (EAnd (l1, l2))



{-  At size 0, it should return an empty list.
    At other sizes, it should call the provided function to get expressions of
    given sizes. The resulting expression size should be n and should be an
    "or" expression.

    TO GET FULL CREDIT, YOU MUST USE DO SYNTAX WITH THE LIST MONAD.

    HINT: numbersplit will be useful here.
-}
orExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
orExpressionsAtSize f n =
    do
        numberPairs <- numberSplit (n-1)
        l1 <- f (fst numberPairs)
        l2 <- f (snd numberPairs)
        return (EOr (l1, l2))

{-  This should simply call andExpressionsAtSize, orExpressionsAtSize,
    notExpressionsAtSize, varExpressionsAtSize, and baseExpressionsAtSize,
    with the appropriate arguments, and concatenate the results.
-}
expressionsAtSize :: Context -> Int -> [Expression]
expressionsAtSize c 1 =
    baseExpressionsAtSize 1 ++ varExpressionsAtSize c 1
expressionsAtSize c n =
    andExp ++ orExp ++ notExp
    where
        andExp = andExpressionsAtSize (expressionsAtSize c) n
        orExp = orExpressionsAtSize (expressionsAtSize c) n
        notExp = notExpressionsAtSize (expressionsAtSize c) n


{-  Check whether a given expression satisfies the provided examples.

    HINT: the "all" function will be useful here.
-}
expressionSatisfiesExamples :: Examples -> Expression -> Bool
expressionSatisfiesExamples (Examples lst) exp =
    all (== True) [evaluate (fst example) exp == snd example| example <- lst]



{-  Generate an expression that satisfies the examples. Check if there are 
    examples at size 1, then at size 2, ... until either there are no 
    expressions at size max or until an expression is found that satisfies the
    examples.

    HINT: Use a helper function
    HINT: The "find" function will be useful here
    HINT: The "evaluate" function will be useful here
-}
generator :: Context -> Examples -> Int -> Maybe Expression
generator c examples n = 
    find (expressionSatisfiesExamples examples) (helper c examples n)

    where 
        helper :: Context -> Examples -> Int -> [Expression]
        helper c examples n =
            do
                num <- [1 .. n]
                expressionsAtSize c num



