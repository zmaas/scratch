#!/usr/bin/env stack
-- Memoized Fibonacci Sequence
fib :: Int -> Integer
fib = (map mFib [0..] !!)
        where mFib 0 = 1
              mFib 1 = 1
              mFib x = fib (x - 1) + fib (x - 2)

main = print (map fib [1..10000])
