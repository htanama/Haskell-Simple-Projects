-- Function to check if a number is prime
isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False -- Numbers less than or equal to 1 are not prime
  | n == 2 = True  -- 2 is the only even prime number
  | n `mod` 2 == 0 = False -- Other even numbers are not prime
  | otherwise = all (\i -> n `mod` i /= 0) [3, 5 .. floor (sqrt (fromIntegral n))]
  -- Check divisibility only by odd numbers up to the square root of n

-- Define a function to find the first prime in a list
findFirstPrime :: [Int] -> Maybe Int
findFirstPrime []       = Nothing
findFirstPrime (x:xs)
  | isPrime x = Just x
  | otherwise = findFirstPrime xs

-- Main function to search for and display a prime number
main :: IO ()
main = do
  putStrLn "Enter a list of numbers separated by spaces:"
  input <- getLine
  let nums = map read (words input) :: [Int]
  case findFirstPrime nums of
    Nothing -> putStrLn "No prime found in the list."
    Just prime -> putStrLn $ "The first prime number in the list is: " ++ show prime
