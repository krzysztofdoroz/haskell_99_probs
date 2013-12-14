{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

--
myLast :: [a] -> a
myLast [a] = a
myLast (x:xs) = myLast xs

--
myButLast :: [a] -> a
myButLast [a,b] = a
myButLast (x:xs) = myButLast xs
--

elementAt :: [a] -> Int -> a
elementAt (x:xs) 0 = x
elementAt (x:xs) n = elementAt xs (n-1)
--

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs)  = 1 + myLength xs
--

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]

--
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome l = l == (myReverse l)

--
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress l = compress2 l []
   where
    compress2 [] acc = myReverse acc
    compress2 (x:xs) acc = if not (null acc) && x == head acc
                            then compress2 xs acc
                            else compress2 xs (x:acc)


exeMain = do
        print (myLast [1,2,3,4])
        print (myLast ['a','v'])
        print (myButLast [1,2,3,4])
        print (elementAt [1,2,3,4] 1)
        print (myReverse [1,2,3,4])
        print (myReverse "Mary had a little lamb")
        print (isPalindrome [1,3,2])
        print (isPalindrome "madamimadam")
        print (isPalindrome [1])
        print (compress [1,2,2])
        print (compress "aaaabccaadeeee")

--map2 :: (a ->b) -> a -> b
--map2 f [] = []
--map2 f(x:xs) = f x : map2 f xs

--succ function
add :: Int -> Int
add i = 1 + i

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

