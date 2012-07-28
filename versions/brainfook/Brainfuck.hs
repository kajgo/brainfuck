module Brainfuck where

import Data.Char

data Tape = Tape {memBefore, memAfter :: [Int]}
    deriving (Show, Eq)

emptyTape :: Tape
emptyTape = (Tape [] [0])

data Program = Program {before, after :: String}
    deriving (Show, Eq)

data Machine = Machine {
    tape :: Tape,
    output :: String,
    program :: Program }
    deriving (Show, Eq)


increment (Tape before (x:xs)) =
    (Tape before (x+1:xs))
decrement (Tape before (x:xs)) =
    (Tape before (x-1:xs))

forward (Tape before [x]) =
    Tape (before ++ [x]) [0]
forward (Tape before (x:xs)) =
    Tape (before ++ [x]) xs
back l@(Tape [] after) = l
back (Tape before after) =
    Tape (init before) $ last before : after

next (Program before after) =
    Program (before ++ [head(after)]) (tail(after))
prev (Program before after) =
    Program (init(before)) (last(before) : after)

atTape :: Tape -> Int
atTape (Tape _ (cur:_)) = cur

atProgram :: Program -> Char
atProgram (Program before (cur:rest)) = cur

putAtTape :: Tape -> Int -> Tape
putAtTape (Tape before (_:xs)) v = (Tape before (v:xs))

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
runProgram machine@(Machine _ _ (Program _ "")) = return machine
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
      otherwise -> runProgram (Machine tape output (next program))


execute :: String -> IO ()
execute program = do
    machine <- runProgram (Machine emptyTape "" (Program "" program))
    putStrLn $ output(machine) ++ "done!"
