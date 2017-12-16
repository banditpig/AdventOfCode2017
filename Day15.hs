import           Data.Bits
import           Data.Char (intToDigit)
import           Numeric

last16Same :: Integer -> Integer -> Bool
last16Same x y = xor ((.&.) x 65535)  ((.&.) y 65535) == 0

showB :: Integer -> String
showB x = showIntAtBase 2 intToDigit x ""
--generator A uses 16807; generator B uses 48271)
-- Generator A starts with 516
-- Generator B starts with 190  div by 2147483647
sa2, sb2, sa1, sb1 :: Integer
sa2 = genA2 516
sb2 = genB2 190
sa1 = genA1 516
sb1 = genB1 190


-- Generator A looks for values that are multiples of 4.
genA2 :: Integer -> Integer
genA2  x
    | x' `rem` 4 == 0 = x'
    | otherwise = genA2 x'
      where x' = x * 16807 `rem` 2147483647

genA1 :: Integer -> Integer
genA1  x = x * 16807 `rem` 2147483647

-- Generator B looks for values that are multiples of 8.
genB2 :: Integer -> Integer
genB2 x
    | x' `rem` 8 == 0 = x'
    | otherwise = genB2 x'
      where
       x' =  x * 48271 `rem` 2147483647
genB1 :: Integer -> Integer
genB1 x = x * 48271 `rem` 2147483647

valA1, valB1, valA2, valB2 :: [Integer]
valA1 = iterate genA1 sa1
valB1 = iterate genB1 sb1
valA2 = iterate genA2 sa2
valB2 = iterate genB2 sb2

part1, part2 :: Int
part1 = length $  filter (uncurry last16Same ) $ take 40000000 $ zip valA1 valB1
part2 = length $  filter (uncurry last16Same ) $ take 5000000 $ zip valA2 valB2

main :: IO ()
main = do
  print part1
  print part2
