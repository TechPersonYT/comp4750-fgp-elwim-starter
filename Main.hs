import System.Environment
import Control.Monad.State
import Data.Map (empty)
import qualified Scanner
import qualified Parser
import qualified Interpret

runElwim :: String -> IO ()
runElwim file = do
  input <- readFile file
  case Scanner.scan input >>= Parser.parse of
    Left msg -> print msg
    Right ast -> do
      _ <- runStateT (Interpret.execute ast) empty
      return ()

main :: IO ()
main = do
  files <- getArgs
  mapM runElwim files
  return ()
