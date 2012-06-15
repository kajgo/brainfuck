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

printOut (Tape before after) =
    chr (head(after))
readIn (Tape before (_:rest)) char =
    Tape before ((ord char) : rest)

end (Machine (Tape before (0:after)) out program) =
    (Machine (Tape before (0:after)) out (next(program)))
end (Machine tape out program) =
    (Machine tape out (jumpBack program (-1)))
begin (Machine (Tape before (0:after)) out program) =
    (Machine (Tape before (0:after)) out (jumpForward program (-1)))
begin (Machine tape out program) =
    (Machine tape out (next(program)))

next (Program before after) =
    Program (before ++ [head(after)]) (tail(after))

jumpBack :: Program -> Int -> Program
jumpBack (Program before ('[':rest)) 0 =
    (Program before ('[':rest))
jumpBack (Program before ('[':rest)) depth =
    jumpBack (Program (init(before)) ((last(before)):'[':rest)) (depth - 1)
jumpBack (Program before (']':rest)) depth =
    jumpBack (Program (init(before)) ((last(before)):']':rest)) (depth + 1)
jumpBack (Program before after) depth =
    jumpBack (Program (init(before)) ((last(before)):after)) depth

jumpForward :: Program -> Int -> Program
jumpForward (Program before (']':rest)) 0 =
    (Program before (']':rest))
jumpForward (Program before (']':rest)) depth =
    jumpForward (Program (before ++ "]") rest) (depth - 1)
jumpForward (Program before ('[':rest)) depth =
    jumpForward (Program (before ++ "[") rest) (depth + 1)
jumpForward (Program before after) depth =
    jumpForward (Program (before ++ [head(after)]) (tail(after))) depth


runProgram (Machine tape output (Program before ('+' : rest))) =
    runProgram (Machine (increment tape) output (next(Program before ('+' : rest))))
runProgram (Machine tape output (Program before ('-' : rest))) =
    runProgram (Machine (decrement tape) output (next(Program before ('-' : rest))))

runProgram (Machine tape output (Program before ('.' : rest))) = do
    runProgram (Machine tape (output ++ [printOut tape]) (next(Program before ('-' : rest))))

runProgram (Machine tape output (Program before ('>' : rest))) =
    runProgram (Machine (forward tape) output (next(Program before ('>' : rest))))
runProgram (Machine tape output (Program before ('<' : rest))) =
    runProgram (Machine (back tape) output (next(Program before ('<' : rest))))

runProgram (Machine tape output (Program before ('[' : rest))) =
    runProgram (begin (Machine tape output (Program before ('[' : rest))))
runProgram (Machine tape output (Program before (']' : rest))) =
    runProgram (end (Machine tape output (Program before (']' : rest))))

runProgram (Machine tape output (Program before "")) =
    (Machine tape output (Program before ""))
runProgram other =
    Machine (Tape [] []) "" (Program "" "")
--runProgram (Machine tape output (Program before (cur:rest))) =
--    runProgram (Machine tape output (Program (before ++ [cur]) rest))


execute :: String -> IO ()
execute program = do
    let machine = runProgram (Machine (Tape [] [0]) "" (Program "" program))
    if (tape(machine) == Tape [] [])
        then putStr("error!")
        else putStrLn $ output(machine) ++ "done!"
