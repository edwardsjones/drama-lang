module Main(main) where
import System.Environment
import Types
import qualified Tokens as L
import qualified Happy as P
import qualified Interpreter as I

main = do
    s <- getArgs
    fileContents <- readFile $ head s
    ast <- P.parseDrama $ L.alexScanTokens fileContents
    I.evalProgram ast
