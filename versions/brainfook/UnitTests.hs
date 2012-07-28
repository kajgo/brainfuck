import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

anEmptyTape :: Tape
anEmptyTape = (Tape [] [0])

aTapeAtPosition :: [Int] -> Int -> Tape
aTapeAtPosition contents pos = (Tape (take pos contents) (drop pos contents))

contentsOfTape :: Tape -> [Int]
contentsOfTape (Tape before after) = before ++ after

aMachineWithProgram p = (Machine anEmptyTape "" (Program "" p))

main = hspecX $ do


    describe "test increment" $ do
        it "can increment once" $ do
            increment anEmptyTape @?= aTapeAtPosition [1] 0
        it "can increment twice" $ do
            increment ( increment anEmptyTape) @?= aTapeAtPosition [2] 0
        it "can increment twice" $ do
            increment ( increment anEmptyTape) @?= aTapeAtPosition [2] 0

    describe "decrement" $ do
        it "can decrement once" $ do
            decrement (aTapeAtPosition [1] 0) @?= aTapeAtPosition [0] 0

    describe "print" $ do
        it "prints the first item" $ do
            printOut (aTapeAtPosition [65] 0) @?= 'A'

--    describe "read" $ do
--        it "reads a character" $ do
--            readIn anEmptyTape 'A' @?= (aTapeAtPosition [65] 0)

    describe "move forward" $ do
        it "takes one step forward" $ do
            forward (aTapeAtPosition [0, 1] 0) @?= (aTapeAtPosition [0, 1] 1)
        it "takes one step forward and appends zero at end" $ do
            forward anEmptyTape @?= (aTapeAtPosition [0, 0] 1)

    describe "move back" $ do
        it "takes one step back" $ do
            back (aTapeAtPosition [0, 1] 1) @?= (aTapeAtPosition [0, 1] 0)
        it "stops at beginning" $ do
            back anEmptyTape @?= anEmptyTape

    describe "loop" $ do
        it "jumps back on nonzero" $ do
            end (Machine (aTapeAtPosition [0, 1] 1) "" (Program "[" "]")) @?=
               (Machine (aTapeAtPosition [0, 1] 1) "" (Program "" "[]"))
        it "doesn't jump back on zero" $ do
            end (Machine (aTapeAtPosition [0, 0] 1) "" (Program "[" "]")) @?=
               (Machine (aTapeAtPosition [0, 0] 1) "" (Program "[]" ""))
        it "jumps forward on zero" $ do
            begin (Machine (aTapeAtPosition [0, 0] 1) "" (Program "" "[+]")) @?=
               (Machine (aTapeAtPosition [0, 0] 1) "" (Program "[+" "]"))
        it "doesn't jump forward on nonzero" $ do
            begin (Machine (aTapeAtPosition [0, 1] 1) "" (Program "" "[+]")) @?=
               (Machine (aTapeAtPosition [0, 1] 1) "" (Program "[" "+]"))

    describe "nested loop" $ do
        it "jumps back to leftmost" $ do
            end (Machine (aTapeAtPosition [0, 1] 1) "" (Program "[[]" "]")) @?=
                (Machine (aTapeAtPosition [0, 1] 1) "" (Program "" "[[]]"))
        it "jumps forward to rightmost" $ do
            begin (Machine (aTapeAtPosition [0, 0] 1) "" (Program "" "[[]]")) @?=
                (Machine (aTapeAtPosition [0, 0] 1) "" (Program "[[]" "]"))

--    describe "runProgram" $ do
--        it "runs a single increment" $ do
--            tape(runProgram (aMachineWithProgram "+")) @?= (aTapeAtPosition [1] 0)
--        it "runs two increments" $ do
--            tape(runProgram (aMachineWithProgram "++")) @?= (aTapeAtPosition [2] 0)
--        it "increments and decrements back to zero" $ do
--            tape(runProgram (aMachineWithProgram "++--")) @?= anEmptyTape
--        it "prints the first item" $ do
--            output(runProgram (Machine (aTapeAtPosition [65] 0) "" (Program "" "."))) @?= "A"
--        it "moves forward" $ do
--            tape(runProgram(Machine (aTapeAtPosition [65] 0) "" (Program "" ">"))) @?=
--                (aTapeAtPosition [65, 0] 1)
--        it "moves backwards" $ do
--            tape(runProgram(Machine (aTapeAtPosition [65, 65] 1) "" (Program "" "<"))) @?=
--                (aTapeAtPosition [65, 65] 0)
--        it "loops once" $ do
--            tape(runProgram (Machine (aTapeAtPosition [1, 65] 0) "" (Program "" "[>.<-]"))) @?=
--                (aTapeAtPosition [0, 65] 0)
--            output(runProgram (Machine (aTapeAtPosition [1, 65] 0) "" (Program "" "[>.<-]"))) @?=
--                "A"