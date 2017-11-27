module Main where

import qualified Data.Text.IO       as T.IO (readFile)
import           System.Environment (getArgs)
import           Text.Megaparsec    (parse)

import           CommandParser      (commands)
import           Commands           (doCommand, evalCommands)

main :: IO ()
main = do
    [file] <- getArgs
    content <- T.IO.readFile file
    case parse commands "main" content of
        Left  err  -> print err
        Right cmds -> do
            res <- doCommand mempty $ evalCommands cmds
            case res of
                Left  e -> print e
                Right _ -> return ()
