module AdventOfCode where

-- day 1

fuel :: Int -> Int
fuel = max 0 . subtract 2 . flip div 3

modulesFuelRequirement :: [Int] -> Int
modulesFuelRequirement = sum . map fuel

fuelIncFuel :: Int -> Int
fuelIncFuel 0 = 0
fuelIncFuel m = f + fuelIncFuel f
  where
    f = fuel m

modulesFuelRequirementIncFuel :: [Int] -> Int
modulesFuelRequirementIncFuel = sum . map fuelIncFuel

-- day 2
