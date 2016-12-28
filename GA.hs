import Data.Char        --Functions to operate on characters/ints
import Data.List        --Functions to operate on lists (sort, length)
import System.Random    --Functions for generation of random numbers
import BFInterpreter3   --Our BF interpreter!


{-
Generates three infinite lists of random numbers, and then runs the
genetic algorithm using the given parameters. The parameters are:
    -The three lists of random numbers
    -The mutation rate
    -The crossover rate
    -The length of each BF string
    -The population size
    -The desired output
    -The target fitness
-}
main :: IO ()
main = do
    putStrLn "Mutation Rate (0-100): "
    mRate <- readLn
    putStrLn "Crossover Rate (0-100): "
    cRate <- readLn
    putStrLn "Chromosome Length: "
    cLength <- readLn
    putStrLn "Population Size: "
    popSize <- readLn
    putStrLn "Target Output: "
    target <- readLn

    seed1 <- newStdGen
    seed2 <- newStdGen
    seed3 <- newStdGen
    let rand99s = randomRs (0,99) seed1
    let randBFs = randomRs (0,(cLength - 1)) seed2
    let rand7s  = randomRs (0,7) seed3
    --let desired = "h"
    let result  = runGA rand99s randBFs rand7s mRate cRate cLength popSize target (fitness (toInt target) target)

    putStrLn ("Num Generations: " ++ show (snd result))
    putStrLn ("BF Code: " ++ getBF (fst result))
    putStrLn ("Output: " ++ toString (getOut (fst result)))


{-
Data structure for an "Individual". Holds a String, a list
of ints, and an Int. The string is a string of BF code,
the list of ints is the output of that code, and the
single int is the fitness.
-}
data Individual = Individual String [Int] Int
    deriving Show

--Determines if two individuals are equal based on their code.
instance Eq Individual where
    (Individual s _ _) == (Individual s' _ _) = s == s'

--Compares individuals based off of fitness.
instance Ord Individual where
    (Individual _ _ f) <= (Individual _ _ f') = not (f <= f')

--Returns the BF code of an individual.
getBF :: Individual         --The individual to be examined
      -> String             --Individual's BF code
getBF (Individual bf _ _) = bf

--Returns the output of an individual.
getOut :: Individual        --The individual to be examined
       -> [Int]             --Individual's output (result of running the BF code)
getOut (Individual _ out _) = out

--Returns the fitness of an individual.
getFit :: Individual        --The individual to be examined
       -> Int               --Individual's fitness
getFit (Individual _ _ fit) = fit

{-
Functions involved in running a genetic algorithm and obtaining a result.
-}
--Run the genetic algorithm with a particular set of parameters
runGA :: [Int]              --List of random ints between 0 and 99
      -> [Int]              --List of random ints between 0 and BF string length
      -> [Int]              --List of random ints between 0 and 7
      -> Int                --Mutation rate (between 0 and 99)
      -> Int                --Crossover rate (between 0 and 99)
      -> Int                --Chromosome length
      -> Int                --Population size
      -> String             --Target string from the BF program
      -> Int                --Target fitness result
      -> (Individual, Int)  --Result of the GA!
runGA rand99 randBF rand7 mRate cRate cLen popS s fFit
            = runPop rand99 randBF (drop (popS*cLen) rand7) mRate cRate s fFit 0 pop
                where
                    pop = sort (generatePop rand7 popS cLen s)

--Does a thing where it does stuff.
runPop :: [Int]             --List of random ints between 0 and 99
       -> [Int]             --List of random ints between 0 and BF string length
       -> [Int]             --List of random ints between 0 and 7
       -> Int               --Mutation rate (between 0 and 99)
       -> Int               --Crossover rate (between 0 and 99)
       -> String            --Target string from BF program
       -> Int               --Target fitness result
       -> Int               --Number of generations
       -> [Individual]      --The population to be run
       -> (Individual, Int) --Result of running!
runPop rand99 randBF rand7 mRate cRate s fFit gens pop@(ind:inds) 
            | getFit ind == fFit = (ind, gens)
            | otherwise          = runPop (drop r99 rand99) (drop bfs randBF) (drop mUsed rand7) mRate cRate s fFit (gens+1) newPop
        where
            pickPop         = pickPopParent rand99 20 pop
            picked          = fst pickPop
            pUsed           = snd pickPop
            cross           = crossPop rand99 randBF cRate picked
            cUsed           = snd cross
            mut             = mutPop (drop cUsed rand99) (drop cUsed randBF) rand7 mRate (fst cross)
            mUsed           = snd mut
            newChromosomes  = fst mut
            newInds         = evalPop newChromosomes s
            newPop          = take 20 (sort (head pop:newInds))
            r99             = pUsed + cUsed + mUsed
            bfs             = cUsed + mUsed


{-
Functions involved in generating the starting population.
-}
--Generates a chromosome of length chromosome length
generateChromosome :: [Int]     --List of random ints between 0 and chromosome length
                   -> Int       --Chromosome length
                   -> String    --Resulting chromosome
generateChromosome _ 0  = []
generateChromosome [] _ = []
generateChromosome (randBF:randBFs) i = pickedOne randBF ",.[]<>+-":generateChromosome randBFs (i-1)


--Generates a population of n individuals, each with chromosome length i
generatePop :: [Int]            --List of random ints between 0 and chromosome length
            -> Int              --Population size
            -> Int              --Chromosome length
            -> String           --Target string (used to get fitness)
            -> [Individual]     --The resulting population
generatePop _  0 _ _      = []
generatePop [] _ _ _      = []
generatePop randBFs n i s = (Individual code out fit):generatePop (drop i randBFs) (n-1) i s
                            where
                                code = generateChromosome randBFs i
                                out  = runCode code
                                fit  = fitness out s

{-
Functions involved in generating a new population.
-}
--Selects two individuals from a population using roulette wheel selection
selectTwo :: [Int]                              --List of random ints between 0 and 99
          -> [Int]                              --RangeL list (proportions)
          -> Int                                --Number of random numbers consumed
          -> [Individual]                       --Population being operated on
          -> ((Individual, Individual), Int)    --Picked individuals, and total numbers consumed
selectTwo (x:y:xs) rangeL used inds | i1 == i2  = selectTwo xs rangeL (used+2) inds
                                    | otherwise = ((i1, i2), used+2)
                                where
                                    i1 = pickedOne (pickedOne x rangeL) inds
                                    i2 = pickedOne (pickedOne y rangeL) inds

--Selects a new population (pairs of non-equal individuals) from a population
pickNewPop :: [Int]                 --List of random ints between 0 and 99
           -> [Int]                 --rangeL list (proportions)
           -> Int                   --Number of individuals to pick
           -> Int                   --Number of random numbers consumed
           -> [Individual]          --Population being operated on
           -> ([Individual], Int)   --Picked individuals, and total numbers consumed
pickNewPop rand99 rangeL used n inds 
            | n > 0 = (fst (fst two):snd (fst two):fst (pickNewPop (drop num rand99) rangeL newUsed (n-2) inds), newUsed)
            | otherwise = ([], used)
                where
                    two = selectTwo rand99 rangeL 0 inds
                    num = snd two
                    newUsed = used + num

--Calculates the rangeL needed by pickNewPop, then selects a population
pickPopParent :: [Int]                  --List of random ints between 0 and 99
              -> Int                    --Number of individuals to pick
              -> [Individual]           --Population being selected from
              -> ([Individual], Int)    --New population, and total numbers consumed
pickPopParent rand99 n inds = pickNewPop rand99 rangeL 0 n inds
                                where
                                    t = totalFit inds
                                    p = propFit inds t
                                    rangeL = rangeList p 0

--Breeds a population and outputs the resulting BF code
crossPop :: [Int]               --List of random ints between 0 and 99
         -> [Int]               --List of random ints between 0 and chromosome length
         -> Int                 --Crossover rate
         -> [Individual]        --Population being operated on
         -> ([String], Int)     --List of new BF code after crossover events, and total numbers consumed
crossPop _ _ _ []         = ([], 0)
crossPop (r99:r99s) (rBF:rBFs) cRate (x:y:xs) 
            | r99 < cRate = (crossover rBF (getBF x) (getBF y):crossover rBF (getBF y) (getBF x):fst next, (snd next)+1)
            | otherwise   = (getBF x:getBF y:fst next, (snd next)+1) 
                where
                    next = crossPop r99s rBFs cRate xs

--Crosses two strings of BF code at a specified location
crossover :: Int            --Random int between 0 and chromosome length
          -> String         --BF code 1
          -> String         --BF code 2
          -> String         --Cross of BF code 1 and 2 at i
crossover 0 _ ys = ys
crossover i (x:xs) (y:ys) = x:(crossover (i-1) xs ys)

--Mutates a list of BF code
mutPop :: [Int]             --List of random ints between 0 and 99
       -> [Int]             --List of random ints between 0 and chromosome length
       -> [Int]             --List of random ints between 0 and 7
       -> Int               --Mutation rate
       -> [String]          --BF code being operated on
       -> ([String], Int)   --Mutated BF strings, and total numbers consumed
mutPop _ _ _ _ []                                                = ([],0)
mutPop (r99:r99s) (rBF:rBFs) (r7:r7s) mRate (x:xs) | r99 < mRate = (mutate rBF r7 x:fst next, (snd next)+1)
                                                   | otherwise   = (x:fst next, (snd next)+1) 
                                                        where
                                                            next = mutPop r99s rBFs r7s mRate xs

--Mutates BF code with specified character at given position
mutate :: Int           --Random int between 0 and chromosome length
       -> Int           --Random int between 0 and 7
       -> String        --BF code
       -> String        --Mutated BF code
mutate 0 char (c:cs) = (pickedOne char ",.[]<>+-"):cs
mutate i char (c:cs) = c:mutate (i-1) char cs

--Evaluates a bunch of BF programs and generates a population
evalPop :: [String]         --List of chromosomes (BF strings)
        -> String           --Target output
        -> [Individual]     --The resulting population
evalPop [] _     = []
evalPop (x:xs) s = (Individual x out fit):evalPop xs s where
                                        out = runCode x
                                        fit = fitness out s

{-
Functions involved in calculating proportion of a population's fitness
belonging to each individual. These support roulette wheel selection.
-}
--Calculates total fitness of a population
totalFit :: [Individual]        --The population being operated on
         -> Int                 --Total fitness of the population
totalFit [] = 0
totalFit (i:is) = (getFit i) + (totalFit is)


--Calculates proportions of total fitness belonging to each individual in a population
propFit :: [Individual]         --Population being operated on
        -> Int                  --Total fitness of the population
        -> [Int]                --List of proportion of total fitness belonging to each individual
propFit [] _ = []
propFit (i:is) totalF = floor ((fromIntegral (getFit i)) / (fromIntegral (totalF)) * 100):propFit is totalF

--Repeats a given item a specified number of times
repeatN :: Int          --Item to be repeated
        -> Int          --Number of times to repeat it
        -> [Int]        --List of n repeats of the item
repeatN 0 n = []
repeatN x n = n:repeatN (x-1) n

--Generates a list of indices, where number of repeats of an index is proportional to that individual's fitness
rangeList :: [Int]          --List of integers (proportions of fitness)
          -> Int            --Starting individual (always 0)
          -> [Int]          --List of indices (100 items)
rangeList (x:xs) n = (repeatN x n) ++ (rangeList xs (n+1))
rangeList [] _ = []


{-
Fitness function!
-}
--Calculates fitness of a list of ints (corresponding to a string)
fitness :: [Int]            --Output of running a BF program
        -> String           --Target string
        -> Int              --Fitness
fitness (x:xs) (c:cs) = 256 - abs((x - (ord c))) + (fitness xs cs)
fitness [] _ = 0
fitness _ [] = 0

{-
Support functions
-}
--Takes the item from specified index in a list
pickOne :: Int          --Random number between 0 and list length
        -> [a]          --List item is being selected from
        -> a            --Selected item
pickOne 0 (x:xs) = x
pickOne i (x:xs) = pickOne (i-1) xs

--Ensures the pickOne is not run with invalid inputs
pickedOne :: Int          --Random number between 0 and list length
          -> [a]          --List item is being selected from
          -> a            --Selected item
pickedOne i l | i < length l = pickOne i l
           | otherwise = pickedOne (i-1) l

--Converts a list of ints to corresponding string
toString :: [Int]           --List of ints (output of BF program)
         -> String          --Corresponding string
toString (x:xs) = (chr x):toString xs
toString [] = []

--Converts a string to corresponding list of ints
toInt :: String             --Input string
      -> [Int]              --Corresponding list of ints
toInt (c:cs) = ord c:toInt cs
toInt [] = []