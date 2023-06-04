{-# LANGUAGE TypeApplications #-}
module Interpret where
import Cleff
import Cleff.Trace
import Cleff.Output
import Ast
import Data.Coerce
-- TODO interpret the Ast
import Effects.Repl (Repl, writeRepl)
import Effects.Fail
import Data.IORef (IORef)
import Effects.Database
import Database
import Prelude hiding (fail)

-- TODO consider also add a Fail..?
type InterpretEffectStack = '[ProgEnv, Trace, Fail]


-- TODO Interpret stuff in the repl - the actual interpret is different prob..
interpretExpr :: InterpretEffectStack :>> es => Expr -> Eff es ()
interpretExpr ast =
  case ast of
    ATerm term ->
      case term of
        N (Name n) -> do
          s <- lookupCon n
          case s of
            Nothing -> trace "Name not found"
            Just s -> trace ("found: " <> show s)
        A a -> trace (show a) -- an agent evaluates to itself.. for now .. TODO eval an agent to normal form
    ACon con -> interpretCon con -- look up things in the db..
    ARule rule -> insertRule rule >> trace ("Inserted rule: " <> show rule) -- rules are just inserted to the db

-- TODO connections between different things, like names and agents need to be implemented..

interpretCon :: InterpretEffectStack :>> es => Connection -> Eff es ()
interpretCon con@(Conn (N n) (A agent)) = trace ("inserted " <> show con <> "\ninto the db") >>  insertCon (coerce n) agent
interpretCon con@(Conn (A agent) (N n)) = trace ("inserted " <> show con <> "\ninto the db") >> insertCon (coerce n) agent
-- connections between names - look up the right hand-side name, and reassign
interpretCon (Conn (N n1) (N n2)) = fail Unimpl

-- TODO connections between agents need to run interaction rules..
interpretCon (Conn (A a1) (A a2)) = do
  let aid1 = aid a1
      aid2 = aid a2
  trace "unimplemented.."
  
normalizeAgent :: InterpretEffectStack :>> es => Agent -> Eff es Agent
normalizeAgent = error "unimplemented"




-- TODO write this in the repl effect
runInterpretExpr :: (IOE :> es) => IORef ProgDB -> Expr -> Eff es ()
runInterpretExpr dbRef s = runFailIO $ runTraceStdout $ runProgEnvRef dbRef $ interpretExpr s

runInterpretExprRepl ::  Repl :> es => Expr -> Eff es ()
runInterpretExprRepl s = runFailRepl $ runTraceRepl $ runProgRepl $ interpretExpr s

runTraceRepl :: Repl :> es => Eff (Trace : es) ~> Eff es
runTraceRepl = interpret \case
  Trace s -> writeRepl s



