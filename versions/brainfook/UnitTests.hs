import Brainfuck
import Test.Hspec.HUnit
import Test.Hspec.Monadic
import Test.HUnit

anEmptyTape = (Tape [] [0])
aMachineWithProgram p = (Machine anEmptyTape "" (Program "" p))

main = hspecX $ do


    describe "test increment" $ do
        it "can increment once" $ do
            increment anEmptyTape @?= (Tape [] [1])
        it "can increment twice" $ do
            increment ( increment anEmptyTape) @?= (Tape [] [2])
        it "can increment twice" $ do
            increment ( increment anEmptyTape) @?= (Tape [] [2])

    describe "decrement" $ do
        it "can decrement once" $ do
            decrement (Tape [] [1]) @?= (Tape [] [0])

    describe "print" $ do
        it "prints the first item" $ do
            printOut (Tape [] [65]) @?= 'A'

    describe "read" $ do
        it "reads a character" $ do
            readIn (Tape [] [0]) 'A' @?= (Tape [] [65])

    describe "move forward" $ do
        it "takes one step forward" $ do
            forward (Tape [] [0, 1]) @?= (Tape [0] [1])
        it "takes one step forward and appends zero at end" $ do
            forward (Tape [] [0]) @?= (Tape [0] [0])

    describe "move back" $ do
        it "takes one step back" $ do
            back (Tape [0] [1]) @?= (Tape [] [0, 1])
        it "stops at beginning" $ do
            back (Tape [] [0]) @?= (Tape [] [0])

    describe "loop" $ do
        it "jumps back on nonzero" $ do
            end (Machine (Tape [0] [1]) "" (Program "[" "]")) @?=
               (Machine (Tape [0] [1]) "" (Program "" "[]"))
        it "doesn't jump back on zero" $ do
            end (Machine (Tape [0] [0]) "" (Program "[" "]")) @?=
               (Machine (Tape [0] [0]) "" (Program "[]" ""))
        it "jumps forward on zero" $ do
            begin (Machine (Tape [0] [0]) "" (Program "" "[+]")) @?=
               (Machine (Tape [0] [0]) "" (Program "[+" "]"))
        it "doesn't jump forward on nonzero" $ do
            begin (Machine (Tape [0] [1]) "" (Program "" "[+]")) @?=
               (Machine (Tape [0] [1]) "" (Program "[" "+]"))

    describe "nested loop" $ do
        it "jumps back to leftmost" $ do
            end (Machine (Tape [0] [1] ) "" (Program "[[]" "]")) @?=
                (Machine (Tape [0] [1] ) "" (Program "" "[[]]"))
        it "jumps forward to rightmost" $ do
            begin (Machine (Tape [0] [0] ) "" (Program "" "[[]]")) @?=
                (Machine (Tape [0] [0] ) "" (Program "[[]" "]"))

    describe "runProgram" $ do
        it "runs a single increment" $ do
            tape(runProgram (aMachineWithProgram "+")) @?= (Tape [] [1])
        it "runs two increments" $ do
            tape(runProgram (aMachineWithProgram "++")) @?= (Tape [] [2])
        it "increments and decrements back to zero" $ do
            tape(runProgram (aMachineWithProgram "++--")) @?= anEmptyTape
        it "prints the first item" $ do
            output(runProgram (Machine (Tape [] [65]) "" (Program "" "."))) @?= "A"
        it "moves forward" $ do
            tape(runProgram(Machine (Tape [] [65]) "" (Program "" ">"))) @?=
                (Tape [65] [0])
        it "moves backwards" $ do
            tape(runProgram(Machine (Tape [65] [65]) "" (Program "" "<"))) @?=
                (Tape [] [65, 65])
        it "loops once" $ do
            tape(runProgram (Machine (Tape [] [1, 65]) "" (Program "" "[>.<-]"))) @?=
                (Tape [] [0, 65])
            output(runProgram (Machine (Tape [] [1, 65]) "" (Program "" "[>.<-]"))) @?=
                "A"
