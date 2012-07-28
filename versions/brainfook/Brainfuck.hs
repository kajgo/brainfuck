module Brainfuck where

import Data.Char

data Tape = Tape {memBefore, memAfter :: [Int]}
    deriving (Show, Eq)

data Program = Program {before, after :: String}
    deriving (Show, Eq)

data Machine = Machine {
    tape :: Tape,
    output :: String,
    program :: Program }
    deriving (Show, Eq)


increment (Tape before after) =
    Tape (before) ( head(after) + 1 : tail(after) )
decrement (Tape before after) =
    Tape (before) ( head(after) - 1 : tail(after) )

forward (Tape before [x]) =
    Tape (before ++ [x]) [0]
forward (Tape before after) =
    Tape (before ++ [head(after)]) (tail(after))
back (Tape [] after) =
    Tape ([]) (after)
back (Tape before after) =
    Tape (init(before)) (last(before) : after)

next (Program before after) =
    Program (before ++ [head(after)]) (tail(after))
prev (Program before after) =
    Program (init(before)) (last(before) : after)


printOut (Tape before after) =
    chr (head(after))
readIn (Tape before (_ : rest)) = do
    c <- getChar
    return (Tape before ((ord c) : rest))

end (Machine tape@(Tape _ (0:_)) out program) =
    (Machine tape out (next(program)))
end (Machine tape out program) =
    (Machine tape out (jumpBack program (-1)))
begin (Machine tape@(Tape _ (0:_)) out program) =
    (Machine tape out (jumpForward program (-1)))
begin (Machine tape out program) =
    (Machine tape out (next(program)))

atTape :: Tape -> Int
atTape (Tape before (cur:rest)) = cur

atProgram :: Program -> Char
atProgram (Program before (cur:rest)) = cur

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
    machine <- runProgram (Machine (Tape [] [0]) "" (Program "" program))
    if (tape(machine) == Tape [] [])
        then putStr("error!")
        else putStrLn $ output(machine) ++ "done!"
