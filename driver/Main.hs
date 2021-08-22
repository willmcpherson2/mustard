module Main (main) where

import Control.Monad (when)
import Data.Map
import Mustard
import Prelude hiding (lookup)
import System.Environment (getArgs)
import Text.Pretty.Simple
  ( CheckColorTty(NoCheckColorTty)
  , OutputOptions(..)
  , StringOutputStyle(Literal)
  , defaultColorOptionsLightBg
  , pPrintOpt
  )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parse args empty of
    Left msg -> putStrLn msg
    Right args -> handleArgs args

parse :: [String] -> Map String String -> Either String (Map String String)
parse args parsed = case args of
  arg@"--tokens" : args -> parse args (insert arg "" parsed)
  arg@"--sexpr" : args -> parse args (insert arg "" parsed)
  arg@"--bexpr" : args -> parse args (insert arg "" parsed)
  arg@"--ast" : args -> parse args (insert arg "" parsed)
  arg@"--qualified" : args -> parse args (insert arg "" parsed)
  arg@"-c" : source : args -> parse args (insert arg source parsed)
  source : args -> parse args (insert "" source parsed)
  [] -> Right parsed

handleArgs :: Map String String -> IO ()
handleArgs args = case lookup "" args of
  Just filename -> do
    source <- readFile filename
    handleSource args source
  _ -> case lookup "-c" args of
    Just source -> handleSource args source
    _ -> putStrLn
      "no source code was provided. supply a source file or use the -c flag"
  where
    handleSource args source = do
      let stages = mkStages source
      showStage "Tokens:" "--tokens" args tokens stages
      showStage "Sexpr:" "--sexpr" args sexpr stages
      showStage "Bexpr:" "--bexpr" args bexpr stages
      showStage "Ast:" "--ast" args ast stages
      showStage "Qualified:" "--qualified" args qualified stages
    showStage header arg args stage stages = when
      (member arg args)
      (do
        putStr (header ++ "\n\n")
        pPrintOpt NoCheckColorTty options (stage stages)
      )
    options = OutputOptions
      { outputOptionsIndentAmount = 2
      , outputOptionsPageWidth = 100
      , outputOptionsCompact = False
      , outputOptionsCompactParens = True
      , outputOptionsInitialIndent = 0
      , outputOptionsColorOptions = Just defaultColorOptionsLightBg
      , outputOptionsStringStyle = Literal
      }
