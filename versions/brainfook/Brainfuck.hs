module Brainfuck where

import Data.Char

-- Tape is a list containing current focus and a list with the skipped elements in reverse order
type Tape = ([Int], [Int])

emptyTape :: Tape
emptyTape = ([0], [])

data Program = Program {before, after :: String}
    deriving (Show, Eq)

newProgram :: String -> Program
newProgram text = (Program "" (text ++ "\0"))

data Machine = Machine {
    tape :: Tape,
    output :: String,
    program :: Program }
    deriving (Show, Eq)


atTape :: Tape -> Int
atTape (cur:_, _) = cur

atProgram :: Program -> Char
atProgram (Program before (cur:rest)) = cur

putAtTape :: Tape -> Int -> Tape
putAtTape (_:xs, bs) v = (v:xs, bs)

forward (x:[], bs) =
    ([0], x:bs)
forward (x:xs, bs) =
    (xs, x:bs)
back l@(xs, []) = l
back (xs, x:bs) =
    (x:xs, bs)

next (Program before after) =
    Program (before ++ [head(after)]) (tail(after))
prev (Program before after) =
    Program (init(before)) (last(before) : after)


increment :: Tape -> Tape
increment tape =
    putAtTape tape $ atTape tape + 1

decrement :: Tape -> Tape
decrement tape =
    putAtTape tape $ atTape tape - 1

printOut tape = chr $ atTape tape
readIn tape = do
    c <- getChar
    return $ putAtTape tape (ord c)

end (Machine tape out program) =
    case atTape tape of
      0 -> (Machine tape out (next(program)))
      otherwise -> (Machine tape out (jumpBack program (-1)))
begin (Machine tape out program) =
    case atTape tape of
      0 -> (Machine tape out (jumpForward program (-1)))
      otherwise -> (Machine tape out (next(program)))

jumpBack :: Program -> Int -> Program
jumpBack p depth = case (atProgram p, depth) of
                 ('[', 0) -> p
                 ('[', _) -> jumpBack (prev p) (depth - 1)
                 (']', _) -> jumpBack (prev p) (depth + 1)
                 otherwise -> jumpBack (prev p) depth

jumpForward :: Program -> Int -> Program
jumpForward p depth = case (atProgram p, depth) of
                        (']', 0) -> p
                        (']', _) -> jumpForward (next p) (depth - 1)
                        ('[', _) -> jumpForward (next p) (depth + 1)
                        otherwise -> jumpForward (next p) depth


runProgram :: Machine -> IO Machine
runProgram machine@(Machine tape output program) =
    case atProgram program of
      '+' -> runProgram (Machine (increment tape) output (next program))
      '-' -> runProgram (Machine (decrement tape) output (next program))
      '.' -> runProgram (Machine tape (output ++ [printOut tape]) (next program))
      ',' -> do
        readTape <- (readIn tape)
        runProgram (Machine readTape output (next program))
      '>' -> runProgram (Machine (forward tape) output (next program))
      '<' -> runProgram (Machine (back tape) output (next program))
      '[' -> runProgram (begin machine)
      ']' -> runProgram (end machine)
      '\0' -> return machine
      otherwise -> runProgram (Machine tape output (next program))


execute :: String -> IO ()
execute program = do
    machine <- runProgram (Machine emptyTape "" $ newProgram program)
    putStrLn $ output(machine) ++ "done!"
