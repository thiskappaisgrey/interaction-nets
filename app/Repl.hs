module Repl (runRepl) where
import Cleff
import Cleff.Input
import Cleff.Output
import Cleff.State
  
import Parser
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as T
import Control.Monad
import System.Console.Isocline

import System.Exit
import System.IO

data Repl :: Effect where
  ReadRepl :: Repl m String
  WriteRepl :: String -> Repl m ()
  ExitRepl :: Repl m ()
makeEffect ''Repl
  
runReplIO :: IOE :> es => Eff (Repl : es) a -> Eff es a
runReplIO i = do
  liftIO $
    do
      styleDef "kbd" "gray underline"
      styleDef "ic-prompt" "#00A060"
      enableAutoTab True                  -- complete as far as possible
  irepl i
  where
    irepl = interpretIO \case
      ReadRepl    -> do
        m <- readlineMaybe "inet"
        case m of
          Nothing ->
            do
              exitSuccess
              return ""
          Just c -> return c
        
      WriteRepl s -> putFmtLn s
      ExitRepl -> exitSuccess


repl :: Repl :> es => Eff es ()
repl  =
  do
  str <- readRepl
  when (str == ":q") exitRepl
  let s = parse parseExpr "" $ T.pack str
  case s of
    Left bundle -> writeRepl $ (errorBundlePretty bundle)
    Right st -> writeRepl $ show st
  repl

runRepl :: IO ()
runRepl =  runIOE $ runReplIO repl
