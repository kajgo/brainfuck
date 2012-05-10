import Data.Char
import Data.Maybe
import qualified Data.Map as M

--

data Command =
      MoveRight
    | MoveLeft
    | Increment
    | Decrement
    | Print
    | Read
    | LoopStart Int
    | LoopEnd Int
    | NOP
    deriving (Show, Eq)

--

data Tape a = Tape
    { currentPos :: Int
    , values     :: M.Map Int a
    } deriving (Show)

emptyTape = Tape 0 M.empty

tapePut :: Int -> a -> Tape a -> Tape a
tapePut pos value tape = tape { values = newValues }
    where
        newValues = M.insert pos value (values tape)

tapeGet :: Tape a -> a -> a
tapeGet tape def =
    case M.lookup (currentPos tape) (values tape) of
        Just a  -> a
        Nothing -> def

tapeModifyValue :: Tape a -> a -> (a -> a) -> Tape a
tapeModifyValue tape def fn = tape { values = newValues }
    where
        value     = M.findWithDefault def (currentPos tape) (values tape)
        newValues = M.insert (currentPos tape) (fn value) (values tape)

tapeMoveRight :: Tape a -> Tape a
tapeMoveRight = tapeModifyPos inc

tapeMoveLeft :: Tape a -> Tape a
tapeMoveLeft = tapeModifyPos dec

tapeMoveTo :: Int -> Tape a -> Tape a
tapeMoveTo pos = tapeModifyPos (const pos)

tapeModifyPos :: (Int -> Int) -> Tape a -> Tape a
tapeModifyPos fn tape = tape { currentPos = fn (currentPos tape) }

--

inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1

--

type Program = Tape Command

type Data = Tape Int

--

parse :: String -> Program
parse str = foo 0 str emptyTape

foo :: Int -> String -> Program -> Program
foo n []       p = p
foo n ('>':xs) p = foo (n+1) xs (tapePut n MoveRight p)
foo n ('<':xs) p = foo (n+1) xs (tapePut n MoveLeft p)
foo n ('+':xs) p = foo (n+1) xs (tapePut n Increment p)
foo n ('-':xs) p = foo (n+1) xs (tapePut n Decrement p)
foo n ('.':xs) p = foo (n+1) xs (tapePut n Print p)
foo n (',':xs) p = foo (n+1) xs (tapePut n Read p)
foo n ('[':xs) p = let (innerLoop, _:rest) = parseInnerLoop xs
                       start             = n
                       end               = n + length innerLoop + 1

                       p'                = tapePut start (LoopStart end) p
                       p''               = foo (start+1) innerLoop p'
                       p'''              = tapePut end (LoopEnd start) p''
                   in foo (end+1) rest p'''
foo n (x:xs)   p = error $ "unknown symbol: " ++ [x]

parseInnerLoop :: String -> (String, String)
parseInnerLoop str = parseInnerLoop' 0 "" str
parseInnerLoop' n acc []       = error "unexpected end of input"
parseInnerLoop' n acc (x:xs)
    | x == '['            = parseInnerLoop' (n+1) (acc ++ [x]) xs
    | x == ']' && n == 0  = (acc, x:xs)
    | x == ']'            = parseInnerLoop' (n-1) (acc ++ [x]) xs
    | otherwise           = parseInnerLoop' n     (acc ++ [x]) xs

--

run :: Program -> Data -> IO ()
run p d =
    case tapeGet p NOP of
        MoveRight   -> run (tapeMoveRight p) (tapeMoveRight d)
        MoveLeft    -> run (tapeMoveRight p) (tapeMoveLeft d)
        Increment   -> run (tapeMoveRight p) (tapeModifyValue d 0 inc)
        Decrement   -> run (tapeMoveRight p) (tapeModifyValue d 0 dec)
        Print       -> do
                           putStr ((:[]) $ chr $ tapeGet d 0)
                           run (tapeMoveRight p) d
        Read        -> do
                           c <- getChar
                           run (tapeMoveRight p) (tapeModifyValue d 0 (const $ digitToInt c))
        LoopStart n -> if tapeGet d 0 == 0
                           then run (tapeMoveTo n p) d
                           else run (tapeMoveRight p) d
        LoopEnd n   -> if tapeGet d 0 == 0
                           then run (tapeMoveRight p) d
                           else run (tapeMoveTo n p) d
        NOP         -> putStrLn "done!"

--

helloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

loopForever = "+[>+.<]"

main = run (parse loopForever) emptyTape
