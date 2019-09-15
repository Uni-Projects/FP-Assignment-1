-- s1024726 - Johan Urban
-- s1023775 - Paolo Scattolin
-- Assignment 1 : exercise 1.14
-- about: calling the function with two parameters
--	param1 : number of levels to the triangle (lower limit)
--      param2 : should be zero
triangle :: Int -> Int -> String
triangle n 0
   | n == 0 = ""
   | otherwise = take (n-1) (repeat ' ') ++ "*" ++ "\n" ++ triangle (n-1) 1

triangle n x
   | n == 0 = ""
   | otherwise  = take (n-1) (repeat ' ') ++ take (2*x+1) (repeat '*') ++ ['\n'] ++ triangle (n-1) (x+1)

-- faulty function
christmasTree :: Int -> String
christmasTree n 
   | n == 0 = ""
   | otherwise = christmasTree (n-1) ++ triangle (n-1) 0 ++ triangle n 0
