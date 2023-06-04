module Repl (runRepl) where
import Parser
import Text.Megaparsec (parse, errorBundlePretty)
import Interpret (runInterpretExprRepl)
import Effects.Repl
import Cleff
import qualified Data.Text as T
import Control.Monad



repl :: [IOE, Repl] :>> es => Eff es ()
repl  =
    forever $ do
      str <- readRepl
      when (str == ":q") exitRepl
      let s = parse parseExpr "" $ T.pack str
      case s of
        Left bundle -> writeRepl $ (errorBundlePretty bundle)
        Right st ->  do
          writeRepl (show st)
          runInterpretExprRepl st 
      

runRepl :: IO ()
runRepl =  runIOE $ runReplIO repl
