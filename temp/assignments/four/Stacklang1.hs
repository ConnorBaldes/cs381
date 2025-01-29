module Stacklang1 where

-- Abstract syntax
type Prog = [Cmd]
data Cmd = LD Integer | ADD | MULT | DUP deriving Show

-- Stack type
type Stack = [Integer]

-- Semantics of individual operations
semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LD n) s = Just (n : s)  -- Pushes the integer n onto the stack s
semCmd ADD (x:y:s) = Just (x + y : s)  -- Pops the top two elements of the stack, adds them, and pushes the result back onto the stack
semCmd MULT (x:y:s) = Just (x * y : s)  -- Pops the top two elements of the stack, multiplies them, and pushes the result back onto the stack
semCmd DUP (x:s) = Just (x : x : s)  -- Duplicates the top element of the stack
semCmd _ _ = Nothing  -- Returns Nothing if there is an error

-- Semantics of a program
run :: Prog -> Stack -> Maybe Stack
run [] s = Just s  -- If there are no commands, return the current stack
run (c:p) s = case semCmd c s of  -- Apply the first command to the current stack
                Just s' -> run p s'  -- If semCmd returns a new stack, recursively apply the remaining commands to the new stack
                Nothing -> Nothing  -- If semCmd returns Nothing, return Nothing

-- Test cases
-- stack1 = [1, 2, 3, 4, 5] 
-- test1 = [LD 3,DUP,ADD,DUP,MULT]  
-- test2 = [LD 3,ADD] 
-- test3 = [] 
-- test4 = [ADD, ADD, ADD, ADD]
