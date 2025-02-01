-- Connor Baldes HW5

module HW5sol where

import HW5types
import Data.Maybe

-- Returns the rank of a given command.
-- The rank of a command is the change in stack height resulting from its execution.
rankC :: Cmd -> CmdRank
rankC ADD = (2, 1)   -- Takes two Ints from the stack and pushes their sum. Stack changes by -1.
rankC MULT = (2, 1)  -- Takes two Ints from the stack and pushes their product. Stack changes by -1.
rankC (LDI _) = (0, 1)  -- Pushes an Int onto the stack. Stack changes by 1.
rankC (LDB _) = (0, 1)  -- Pushes a Bool onto the stack. Stack changes by 1.
rankC LEQ = (2, 1)   -- Takes two Ints from the stack and pushes whether the first is less than or equal to the second. Stack changes by -1.
rankC DUP = (1, 2)   -- Duplicates the top element of the stack. Stack changes by 1.
rankC DEC = (1, 1)   -- Decrements the top Int on the stack by 1. Stack remains the same.
rankC (SWAP) = (2, 2)  -- Swaps the positions of the top two elements on the stack. Stack remains the same.
rankC (POP n) = (n, 0)  -- Removes the top n elements from the stack. Stack changes by -n.
rankC (IFELSE prog1 prog2) = 
    let r1 = rankP prog1 0
        r2 = rankP prog2 0
    in if isNothing r1 || isNothing r2 then (0, 0) 
       else (1, min (fromJust r1) (fromJust r2))
     -- Takes a Bool from the top of the stack and executes one of the given programs depending on the value of the Bool. 
     -- The rank is the minimum of the ranks of the two programs, plus 1 if the top element of the stack is a Bool. 
     -- If either program has an undefined rank, returns (0, 0).

-- Returns the rank of a program given a starting rank.
rankP :: Prog -> Rank -> Maybe Rank
rankP prog r = rank prog r

-- Returns the rank of a program given a starting rank.
rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r  -- Empty program leaves stack unchanged.
rank (cmd:cmds) r = case (rankC cmd) of 
                        (n, m) -> if r >= n 
                                  then rank cmds (r - n + m)  -- If the current stack height is at least n, 
                                                            -- simulate the command and proceed with the remaining commands.
                                  else Nothing  -- If the current stack height is less than n, returns Nothing indicating a runtime error.

-- Executes a program on a given stack.
run :: Prog -> Stack -> Result
run p s = let r = rankP p (length s)
          in if isNothing r then RankError  -- If rankP returns Nothing, indicates a runtime error.
             else semCmd p s

-- Executes a given program with an initial stack, returning 'Result'.
-- A program is a list of commands (represented by the 'Cmd' data type) to be executed in order.
-- The stack is a list of 'Item's, either 'I' for integers or 'B' for booleans.
-- The 'Result' is either an 'A' containing the final stack, or an error.
semCmd :: Prog -> Stack -> Result

-- Executes an empty program with the given stack, returning 'Result'.
-- If the program is empty, there are no more commands to execute, so the final stack is returned.
semCmd [] s = A s

-- Executes the first command in a program with the given stack, returning 'Result'.
-- If the program is not empty, the first command is executed and the program is updated to exclude the first command.
-- The stack is also updated based on the command that was executed.
-- Depending on the command and the current state of the stack, an error may occur.
semCmd (c:cs) s = case c of
  -- Load an integer onto the stack.
  LDI n -> semCmd cs (I n : s)
  -- Load a boolean onto the stack.
  LDB b -> semCmd cs (B b : s)
  -- Add the top two integers on the stack and replace them with the result.
  ADD -> case s of
    (I x2:I x1:xs) -> semCmd cs (I (x1 + x2) : xs)
    _ -> RankError
  -- Multiply the top two integers on the stack and replace them with the result.
  MULT -> case s of
    (I x2:I x1:xs) -> semCmd cs (I (x1 * x2) : xs)
    _ -> TypeError
  -- Compare the top two integers on the stack and replace them with a boolean indicating if x1 is less than or equal to x2.
  LEQ -> case s of
    (I x2:I x1:xs) -> semCmd cs (B (x1 <= x2) : xs)
    _ -> RankError
  -- Duplicate the top item on the stack.
  DUP -> case s of
    (x:xs) -> semCmd cs (x:x:xs)
    _ -> RankError
  -- Decrement the top integer on the stack by 1.
  DEC -> case s of
    (I x:xs) -> semCmd cs (I (x - 1) : xs)
    _ -> TypeError
  -- Swap the top two items on the stack.
  SWAP -> case s of
    (x2:x1:xs) -> semCmd cs (x1:x2:xs)
    _ -> RankError
  -- Pop the top n items off the stack.
  POP n -> semCmd cs (drop n s)
  -- Conditional execution: if the top item on the stack is true, execute the first program, otherwise execute the second program.
  IFELSE p1 p2 -> case s of
    (B b:xs) -> if b then semCmd (p1 ++ cs) xs else semCmd (p2 ++ cs) xs
    _ -> TypeError
