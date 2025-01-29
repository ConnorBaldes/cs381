module Stacklang2 where

-- define the program type
type Prog = [Cmd]

-- define the stack type
type Stack = [Either Bool Integer]

-- define the command type
data Cmd = LDI Integer
         | LDB Bool
         | LEQ
         | ADD
         | MULT
         | DUP
         | IFELSE Prog Prog
         | DEC
         | SWAP
         | POP Int
         deriving Show

-- semantics of individual commands
semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LDI n) s = Just (Right n : s)  -- load integer onto the stack
semCmd (LDB b) s = Just (Left b : s)   -- load boolean onto the stack
semCmd ADD (Right n1 : Right n2 : ns) = Just (Right (n1 + n2) : ns) -- add top two integers on the stack
semCmd ADD _ = Nothing -- ADD produces an error if the stack contains fewer than two elements
semCmd MULT (Right n1 : Right n2 : ns) = Just (Right (n1 * n2) : ns) -- multiply top two integers on the stack
semCmd MULT _ = Nothing -- MULT produces an error if the stack contains fewer than two elements
semCmd DUP (x : xs) = Just (x : x : xs) -- place a copy of the top element of the stack onto the stack
semCmd DUP _ = Nothing -- DUP produces an error if the stack is empty
semCmd LEQ (Right n1 : Right n2 : ns) = Just (Left (n1 <= n2) : ns) -- check if top two integers on the stack satisfy the less-than-or-equal-to relation
semCmd LEQ _ = Nothing -- LEQ produces an error if the stack contains fewer than two integers
semCmd (IFELSE p1 p2) (Left b : ns) = if b then run p1 (ns) else run p2 (ns) -- conditional execution of program p1 or program p2 based on boolean on the top of the stack
semCmd (IFELSE _ _) _ = Nothing -- IFELSE produces an error if the top element of the stack is not a boolean

-- semantics of a program
run :: Prog -> Stack -> Maybe Stack
run [] s = Just s -- if the program is empty, return the stack
run (c:cs) s = case semCmd c s of -- evaluate the command
                  Just s' -> run cs s' -- if semCmd succeeds, continue evaluating the rest of the program
                  Nothing -> Nothing -- if semCmd fails, return an error



-- stack1 :: Stack
-- stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
-- stack2 :: Stack
-- stack2 = [Left True, Right 3]
-- test1 = [LDI 3, DUP, ADD, DUP, MULT]
-- test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
-- test3 = [LEQ]
-- test4 = [ADD, ADD, MULT, DUP]
-- test5 = [LEQ, IFELSE [] [], LDI 9]
-- test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
-- test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
-- test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]