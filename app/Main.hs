module Main where

import AdventOfCode (findNounVerbMatchingAnswer, modulesFuelRequirement, modulesFuelRequirementIncFuel, processParametrizedProgram)

main :: IO ()
main = do
  day1
  day2

day1 :: IO ()
day1 =
  let modulesWeight = [137113, 91288, 62216, 61150, 143536, 69244, 102261, 105683, 58305, 67377, 107379, 108666, 56279, 123299, 120794, 60286, 112665, 144945, 100039, 60631, 77509, 106891, 103638, 132144, 119960, 96479, 131631, 105498, 124620, 88703, 101268, 72720, 135531, 108871, 90019, 129257, 69947, 69968, 104725, 95262, 119107, 111562, 81709, 102441, 129733, 84750, 101748, 107232, 113844, 115357, 125062, 83869, 69129, 79132, 144282, 115941, 144188, 58559, 92455, 135538, 146503, 142974, 73517, 112043, 143187, 130617, 144656, 114329, 130205, 92973, 134265, 120776, 62569, 145143, 131663, 130428, 121409, 109042, 111748, 99222, 102198, 63934, 130811, 139884, 107805, 107306, 140757, 149374, 119437, 131554, 55182, 69234, 54593, 92531, 69679, 111405, 143524, 66057, 93150, 53854]
   in do
        putStrLn "day1, fuel requirement"
        print . modulesFuelRequirement $ modulesWeight

        putStrLn "day1, fuel requirement including fuel"
        print . modulesFuelRequirementIncFuel $ modulesWeight

day2 :: IO ()
day2 =
  let gravityAssistProgram = [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 10, 19, 1, 19, 6, 23, 2, 23, 13, 27, 1, 27, 5, 31, 2, 31, 10, 35, 1, 9, 35, 39, 1, 39, 9, 43, 2, 9, 43, 47, 1, 5, 47, 51, 2, 13, 51, 55, 1, 55, 9, 59, 2, 6, 59, 63, 1, 63, 5, 67, 1, 10, 67, 71, 1, 71, 10, 75, 2, 75, 13, 79, 2, 79, 13, 83, 1, 5, 83, 87, 1, 87, 6, 91, 2, 91, 13, 95, 1, 5, 95, 99, 1, 99, 2, 103, 1, 103, 6, 0, 99, 2, 14, 0, 0]
      gravityAssistProgramAnswer = 19690720
   in do
        putStrLn "day2, 1202 Program Alarm"
        print $ processParametrizedProgram 12 2 gravityAssistProgram

        putStrLn "day2, find noun and verb"
        print . head $ findNounVerbMatchingAnswer gravityAssistProgram gravityAssistProgramAnswer
