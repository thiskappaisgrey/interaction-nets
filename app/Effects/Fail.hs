-- error messages and failure
module Effects.Fail where
import Cleff
import Effects.Repl

data Failure =
  Unimpl
  | NameNotFound
message :: Failure -> String
message Unimpl = "Unimplemented ...."
message NameNotFound = "Name not found."

data Fail :: Effect where
  Fail :: Failure -> Fail m ()

makeEffect ''Fail
  
runFailRepl :: Repl :> es => Eff (Fail : es)  ~> Eff es 
runFailRepl = interpret \case
  Fail f -> writeRepl $ message f
  
runFailIO :: IOE :> es => Eff (Fail : es)  ~> Eff es 
runFailIO = interpretIO \case
  Fail f -> putStrLn $ message f



