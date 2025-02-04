module Main
  ( main
  ) where

import Data.Set (Set, fromList)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, getExternalPrint)
import HW5.Action (HIO (runHIO), HiPermission (..))
import HW5.Base (HiError (..), HiExpr (..), HiValue)
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)

-- | A set of permissions granted to the REPL environment.
replPermissions :: Set HiPermission
replPermissions = fromList [AllowRead ..]

-- | Main entry point.
main :: IO ()
main = runInputT defaultSettings repl

-- | REPL loop that continuously reads, evaluates, and prints user input.
repl :: InputT IO ()
repl = do
  command <- getInputLine "hi> "
  case command of
    Nothing      -> pure ()
    Just ":quit" -> pure ()
    Just ""      -> repl
    Just line    -> processInput line >> repl

-- | Processes a single user input line.
processInput :: String -> InputT IO ()
processInput input = do
  output <- getExternalPrint
  case parse input of
    Left err   -> liftIO $ output $ "Parse error: " ++ show err
    Right expr -> evaluateAndPrint expr output

-- | Evaluates an expression and prints the result.
-- If evaluation succeeds, the result is formatted and printed.
-- If evaluation fails, an error message is printed.
evaluateAndPrint :: HiExpr -> (String -> IO ()) -> InputT IO ()
evaluateAndPrint expr output = do
  let computation = eval expr :: HIO (Either HiError HiValue)
  result <- liftIO $ runHIO computation replPermissions
  liftIO $ case result of
    Left err    -> output $ "Evaluation error: " ++ show err
    Right value -> output $ show $ prettyValue value
