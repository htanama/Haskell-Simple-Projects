myMin :: [Int] -> Int
myMin [] = error "List is Empty"
myMin [x] = x
myMin (x:xs) = min x (myMin xs)

main :: IO ()
main = do
  let myList =[50, 39, 20, 37, 44]
  let anotherList = []
  print $ myMin myList
  print $ myMin anotherList
