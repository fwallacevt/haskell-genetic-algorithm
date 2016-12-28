module BFInterpreter3 (runCode) where

{-
An interpreter for BrainFuck. Takes a string that is a BrainFuck program,
and TIMEOUT, prints the output of the program. This interpreter differs 
from others in a few important ways:
    - No error checking; all input will be valid BF programs
    - Runs every program, regardless of if it compiles. It does this by checking
        syntax, and discarding the portion of the program that is "faulty"
    - Implements a timeout so infinite loops exit; this should take a time

The basic interpreter is taken from:
      https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md
-}

--POSSIBILITY OF RUNNING TWO INSTANCES OF INTERPRETER TO ALLOW FOR READING; ONE
--COUNTS THE NUMBER OF READS, AND THE NEXT DOES THEM SEQUENTIALLY AND PASSES
--THE INPUT TO A FUNCTION THAT COMBINES THE TWO OF THESE?

runCode :: String -> [Int]
runCode code = runBrainfuck . parseBrainfuck $ checkSyntax 0 "" code

data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment Char -- anything else
    deriving Show

type BrainfuckSource = [BrainfuckCommand]

checkSyntax :: Int -> String -> String -> String
checkSyntax 0 s (c:cs) = s ++ checkSyntax (updateB 0 c) [c] cs
checkSyntax b s (c:cs) | b < 0     = ""
                       | otherwise = checkSyntax (updateB b c) (s++[c]) cs
checkSyntax b s [] | b == 0    = s
                   | otherwise = ""

updateB :: Int -> Char -> Int
updateB b '[' = b+1
updateB b ']' = b-1
updateB b _   = b

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = map charToBF
  where
    charToBF x = case x of
        '>' -> GoRight
        '<' -> GoLeft
        '+' -> Increment
        '-' -> Decrement
        '.' -> Print
        ',' -> Read
        '[' -> LoopL
        ']' -> LoopR
        c  -> Comment c

data Tape a = Tape [a] -- Left of the pivot element
                    a  -- Pivot element
                   [a] -- Right of the pivot element

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

runBrainfuck :: BrainfuckSource -> [Int]
runBrainfuck = run 2000 emptyTape . bfSource2Tape
    where
        bfSource2Tape [] = Tape [] Print []
        bfSource2Tape (b:bs) = Tape [] b bs
          -- (`run` is defined below)

-- Interpret the command currently focussed on the instruction tape
run :: Int                   -- Counter
    -> Tape Int              -- Data tape
    -> Tape BrainfuckCommand -- Instruction tape
    -> [Int]
run 0 _ _ = []
run n dataTape source@(Tape _ GoRight _) =
      advance n (moveRight dataTape) source
run n dataTape source@(Tape _ GoLeft  _) =
      advance n (moveLeft dataTape) source
run n (Tape l p r) source@(Tape _ Increment  _) =
    advance n (Tape l (p+1) r) source
run n (Tape l p r) source@(Tape _ Decrement  _) =
    advance n (Tape l (p-1) r) source
run n dataTape@(Tape _ p _) source@(Tape _ Print  _) =
    p:advance n dataTape source
run n dataTape@(Tape l _ r) source@(Tape _ Read  _) =
    advance n (moveRight dataTape) source
    --Ignoring reads because we won't need these for our purposes
    --Ask Pete about this; can we read an item and unpack it?
run n dataTape@(Tape _ p _) source@(Tape _ LoopL  _)
    -- If the pivot is zero, jump to the
    -- corresponding LoopR instruction
    | p == 0 = seekLoopR n 0 dataTape source
    -- Otherwise just ignore the `[` and continue
    | otherwise = advance n dataTape source
run n dataTape@(Tape _ p _) source@(Tape _ LoopR  _)
    | p /= 0 = seekLoopL n 0 dataTape source
    | otherwise = advance n dataTape source
run n dataTape source@(Tape _ (Comment _) _) = advance n dataTape source

advance :: Int                   -- Counter
        -> Tape Int              -- Data tape
        -> Tape BrainfuckCommand -- Instruction tape
        -> [Int]
advance _ dataTape (Tape _ _ []) = []
advance n dataTape source = run (n-1) dataTape (moveRight source)

-- Move the instruction pointer left until a "[" is found.
-- The first parameter ("b" for balance) retains the current
-- bracket balance to find the matching partner. When b is 1,
-- then the found LoopR would reduce the counter to zero,
-- hence we break even and the search is successful.
seekLoopR :: Int                   -- Counter
          -> Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape BrainfuckCommand -- Instruction tape
          -> [Int]
seekLoopR n 1 dataTape source@(Tape _ LoopR _) = advance n dataTape source
seekLoopR n b dataTape source@(Tape _ LoopR _) =
    seekLoopR n (b-1) dataTape (moveRight source)
seekLoopR n b dataTape source@(Tape _ LoopL _) =
    seekLoopR n (b+1) dataTape (moveRight source)
seekLoopR n b dataTape source =
    seekLoopR n b dataTape (moveRight source)

seekLoopL :: Int                   -- Counter
          -> Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape BrainfuckCommand -- Instruction tape
          -> [Int]
seekLoopL n 1 dataTape source@(Tape _ LoopL _) = advance n dataTape source
seekLoopL n b dataTape source@(Tape _ LoopL _) =
    seekLoopL n (b-1) dataTape (moveLeft source)
seekLoopL n b dataTape source@(Tape _ LoopR _) =
    seekLoopL n (b+1) dataTape (moveLeft source)
seekLoopL n b dataTape source =
    seekLoopL n b dataTape (moveLeft source)