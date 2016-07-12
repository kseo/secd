module Main where

import Language.MicroLisp
import SECD
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case compile input of
            Left _ -> outputStrLn "Invalid input"
            Right program ->
              let value = eval program
              in outputStrLn $ show value
          loop
