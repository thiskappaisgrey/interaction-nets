module Effects.Database where
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Cleff
import Cleff.State
import Data.IORef
import Effects.Repl
import Database
import Ast


-- Effect definition
data ProgEnv :: Effect where
  InsertCon :: Text -> Agent -> ProgEnv m ()
  LookupCon :: Text -> ProgEnv m (Maybe Agent)
  -- TODO allow names to be freed
  InsertLocalCon :: Text -> Agent -> ProgEnv m ()
  LookupLocalCon :: Text -> ProgEnv m (Maybe Agent)
  ResetLocals :: ProgEnv m ()
  InsertRule :: InteractionRule -> ProgEnv m ()
  LookupRule :: (Text, Text) -> ProgEnv m (Maybe InteractionRule)
makeEffect ''ProgEnv

intProgEnvState ::  Eff (ProgEnv : es)  ~> Eff ((State ProgDB) : es) 
intProgEnvState = reinterpret \case
  InsertCon var agent -> do
    modify $ (\pdb -> let hm = H.insert var agent (consDB pdb) in (pdb { consDB = hm }))
  LookupCon var -> do
    pdb <- get
    return (H.lookup var (consDB pdb))
  InsertLocalCon var agent -> do
    pdb <- get
    let hm = H.insert var agent (localconsDB pdb)
    put (pdb { localconsDB = hm })
  LookupLocalCon var -> do
    pdb <- get
    return (H.lookup var (localconsDB pdb))
  -- InsertLocalCon -> _
  -- LookupLocalCon -> _
  ResetLocals -> modify (\pdb -> (pdb { localconsDB = H.empty }))
  InsertRule rule@(IRule r1 r2 _) -> do
    let id1 = aid r1
        id2 = aid r2
    modify (\pdb -> let hm = H.insert (id1, id2) rule (rules pdb) in (pdb { rules = hm }))
  LookupRule pa -> do
    pdb <- get
    return (H.lookup pa (rules pdb))


-- interpret the progdb effect
runProgEnvState :: Eff (ProgEnv : es) a -> ProgDB -> Eff es (a, ProgDB)
runProgEnvState prog istate = runState istate $ intProgEnvState prog
  
runProgEnvRef :: IOE :> es => IORef ProgDB -> Eff (ProgEnv : es) a ->  Eff es a
runProgEnvRef istate prog  = runStateIORef istate $ intProgEnvState prog

runProgRepl :: Repl :> es => Eff (ProgEnv : es) a -> Eff es ()
runProgRepl prog = do
  db <- getDB
  (_, newdb) <- runProgEnvState prog db
  writeDB newdb 
  
