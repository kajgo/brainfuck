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


jumpBack :: Program -> Int -> Program
jumpBack program@(Program _ ('[':_)) 0 = program
jumpBack program@(Program _ ('[':_)) depth =
    jumpBack (prev program) (depth - 1)
jumpBack program@(Program _ (']':_)) depth =
    jumpBack (prev program) (depth + 1)
jumpBack program depth =
    jumpBack (prev program) depth

jumpForward :: Program -> Int -> Program
jumpForward program@(Program _ (']':_)) 0 = program
jumpForward program@(Program _ (']':_)) depth =
    jumpForward (next program) (depth - 1)
jumpForward program@(Program _ ('[':_)) depth =
    jumpForward (next program) (depth + 1)
jumpForward program depth =
    jumpForward (next program) depth


runProgram (Machine tape output program@(Program _ ('+':_))) =
    runProgram (Machine (increment tape) output (next program))
runProgram (Machine tape output program@(Program _ ('-':_))) =
    runProgram (Machine (decrement tape) output (next program))

runProgram (Machine tape output program@(Program _ ('.':_))) = do
    runProgram (Machine tape (output ++ [printOut tape]) (next program))
runProgram (Machine tape output program@(Program _ (',':_))) = do
    readTape <- (readIn tape)
    runProgram (Machine readTape output (next program))

runProgram (Machine tape output program@(Program _ ('>':_))) =
    runProgram (Machine (forward tape) output (next program))
runProgram (Machine tape output program@(Program _ ('<':_))) =
    runProgram (Machine (back tape) output (next program))

runProgram (Machine tape output program@(Program _ ('[':_))) =
    runProgram (begin (Machine tape output program))
runProgram (Machine tape output program@(Program _ (']':_))) =
    runProgram (end (Machine tape output program))

runProgram (Machine tape output program@(Program _ (_:_))) =
    runProgram (Machine tape output (next program))
runProgram (Machine tape output program@(Program _ "")) =
    return (Machine tape output program)
runProgram other =
    return (Machine (Tape [] []) "" (Program "" ""))


execute :: String -> IO ()
execute program = do
    machine <- runProgram (Machine (Tape [] [0]) "" (Program "" program))
    if (tape(machine) == Tape [] [])
        then putStr("error!")
        else putStrLn $ output(machine) ++ "done!"
