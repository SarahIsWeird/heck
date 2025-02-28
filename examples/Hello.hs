module Hello where

{-
foo :: Int
foo = 1

bar :: Int -> Int
bar 0 = 0
bar n = n + 1

neg :: Int -> Int
neg n = -n

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

sumUntil :: Int -> Int
sumUntil 0 = 0
sumUntil n = n + sumUntil (n - 1)

sumUntil' :: Int -> Int -> Int
sumUntil' 0 acc = acc
sumUntil' n acc = sumUntil' (n - 1) (n + acc)
-}

data List a
  = Empty
  | Cons a (List a)

data Record = Record
  { a :: Int
  , b :: Int
  }

sumRecord :: Record -> Int
sumRecord record = (a record) + (b record)

myAwesomeRecord :: Record
myAwesomeRecord = Record 60 9

sum :: Int
sum = sumRecord myAwesomeRecord
