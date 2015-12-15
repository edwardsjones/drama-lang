module Main(main) where
import System.Environment
import Types
import qualified Tokens as L
import qualified Happy as P
import qualified Interpreter as I

main :: IO ()
main = do
    s <- getArgs
    fileContents <- readFile $ head s
    let ast = P.parseDrama $ L.alexScanTokens fileContents
    print (I.evalProgram ast)
