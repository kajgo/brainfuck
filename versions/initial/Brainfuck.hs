module Brainfuck where

import Data.Char
import Data.Maybe
import qualified Data.Map as M
import System.IO

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
    { currentPos   :: Int
    , values       :: M.Map Int a
    , defaultValue :: a
    } deriving (Show, Eq)

emptyTape :: a -> Tape a
emptyTape def = Tape 0 M.empty def

tapePut :: Int -> a -> Tape a -> Tape a
tapePut pos value tape = tape { values = newValues }
    where
        newValues = M.insert pos value (values tape)

tapeGet :: Tape a -> a
tapeGet tape = M.findWithDefault (defaultValue tape) (currentPos tape) (values tape)

tapeModifyValue :: Tape a -> (a -> a) -> Tape a
tapeModifyValue tape fn = tape { values = newValues }
    where
        value     = M.findWithDefault (defaultValue tape) (currentPos tape) (values tape)
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

type Token = (Int, Char)

parse :: String -> Program
parse str = parseTokens (tokenize str) (emptyTape NOP)

tokenize :: String -> [Token]
tokenize str = zip [0..length f - 1] f
    where
        f = filter (`elem`"<>+-.,[]") str

parseTokens :: [Token] -> Program -> Program
parseTokens []            p = p
parseTokens ((n, '>'):xs) p = parseTokens xs $ tapePut n MoveRight p
parseTokens ((n, '<'):xs) p = parseTokens xs $ tapePut n MoveLeft p
parseTokens ((n, '+'):xs) p = parseTokens xs $ tapePut n Increment p
parseTokens ((n, '-'):xs) p = parseTokens xs $ tapePut n Decrement p
parseTokens ((n, '.'):xs) p = parseTokens xs $ tapePut n Print p
parseTokens ((n, ','):xs) p = parseTokens xs $ tapePut n Read p
parseTokens ((n, '['):xs) p = let (innerLoop, (n',_):rest) = extractInnerLoop xs
                              in parseTokens rest
                               $ tapePut n' (LoopEnd n)
                               $ parseTokens innerLoop
                               $ tapePut n (LoopStart n') p
parseTokens ((n, x):xs)   p = error $ "unknown symbol: " ++ [x]

extractInnerLoop :: [Token] -> ([Token], [Token])
extractInnerLoop tokens = extractInnerLoop' 0 [] tokens
    where
        extractInnerLoop' n acc [] = error "unexpected end of input"
        extractInnerLoop' n acc ((j, x):xs)
            | x == '['             = extractInnerLoop' (n+1) (acc ++ [(j, x)]) xs
            | x == ']' && n == 0   = (acc, (j, x):xs)
            | x == ']'             = extractInnerLoop' (n-1) (acc ++ [(j, x)]) xs
            | otherwise            = extractInnerLoop' n     (acc ++ [(j, x)]) xs

--

run :: Program -> Data -> IO ()
run p d =
    case tapeGet p of
        MoveRight   -> run (tapeMoveRight p) (tapeMoveRight d)
        MoveLeft    -> run (tapeMoveRight p) (tapeMoveLeft d)
        Increment   -> run (tapeMoveRight p) (tapeModifyValue d inc)
        Decrement   -> run (tapeMoveRight p) (tapeModifyValue d dec)
        Print       -> do
                           putChar $ chr $ tapeGet d
                           hFlush stdout
                           run (tapeMoveRight p) d
        Read        -> do
                           c <- getChar
                           run (tapeMoveRight p) (tapeModifyValue d (const $ ord c))
        LoopStart n -> if tapeGet d == 0
                           then run (tapeMoveTo n p) d
                           else run (tapeMoveRight p) d
        LoopEnd n   -> if tapeGet d == 0
                           then run (tapeMoveRight p) d
                           else run (tapeMoveTo n p) d
        NOP         -> putStrLn "done!"

--

execute :: String -> IO ()
execute program = run (parse program) (emptyTape 0)
