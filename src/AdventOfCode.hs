module AdventOfCode where

import Data.List (group)

-- day 1

type Mass = Int

fuel :: Mass -> Mass
fuel = max 0 . subtract 2 . flip div 3

modulesFuelRequirement :: [Mass] -> Mass
modulesFuelRequirement = sum . map fuel

fuelIncFuel :: Mass -> Mass
fuelIncFuel 0 = 0
fuelIncFuel m = f + fuelIncFuel f
  where
    f = fuel m

modulesFuelRequirementIncFuel :: [Mass] -> Mass
modulesFuelRequirementIncFuel = sum . map fuelIncFuel

-- day 2

type ExecutionPointer = Int

type Program = [Int]

type ProgramState = (ExecutionPointer, Program)

processProgram :: Program -> Program
processProgram c = snd . process $ (0, c)

process :: ProgramState -> ProgramState
process (e, c) = case get e of
  1 -> process (e + 4, seti (e + 3) (geti (e + 1) + geti (e + 2)))
  2 -> process (e + 4, seti (e + 3) (geti (e + 1) * geti (e + 2)))
  99 -> (e, c)
  _ -> (e, [])
  where
    get n = c !! n
    set n value = replace n value c
    -- indirect
    geti = get . get
    seti n value = set (get n) value

replace :: Int -> a -> [a] -> [a]
replace n value xs = take n xs ++ [value] ++ drop (n + 1) xs

processParametrizedProgram :: Int -> Int -> Program -> Int
processParametrizedProgram noun verb = head . processProgram . replace 2 verb . replace 1 noun

findNounVerbMatchingAnswer :: Program -> Int -> [Int]
findNounVerbMatchingAnswer program answer =
  [ 100 * noun + verb
    | noun <- [0 .. 99],
      verb <- [0 .. 99],
      processParametrizedProgram noun verb program == answer
  ]

-- day4

neverDecrease :: Int -> Bool
neverDecrease = snd . foldl f ('0', True) . show
  where
    f (prev, res) cur = (cur, res && (prev <= cur))

hasDouble :: Int -> Bool
hasDouble = snd . foldl f (' ', False) . show
  where
    f (prev, res) cur = (cur, res || (prev == cur))

hasDouble' :: Int -> Bool
hasDouble' = any ((>= 2) . length) . group . show

findPasswords :: [Int] -> [Int]
findPasswords = filter hasDouble . filter neverDecrease

hasTrueDouble :: Int -> Bool
hasTrueDouble = any ((== 2) . length) . group . show

findTruePasswords :: [Int] -> [Int]
findTruePasswords = filter hasTrueDouble . filter neverDecrease
