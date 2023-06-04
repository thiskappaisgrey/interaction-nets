module Effects.Repl where
import System.Console.Isocline
import Data.IORef
import Database (emptyProgDB, ProgDB)
import System.Exit
import Cleff


data Repl :: Effect where
  ReadRepl :: Repl m String
  WriteRepl :: String -> Repl m ()
  ExitRepl :: Repl m ()
  GetDB :: Repl m  ProgDB
  WriteDB :: ProgDB -> Repl m ()
makeEffect ''Repl
  
runReplIO :: IOE :> es => Eff (Repl : es) a -> Eff es a
runReplIO i = do
  _ <- liftIO $
    do
      styleDef "kbd" "gray underline"
      styleDef "ic-prompt" "#00A060"
      enableAutoTab True                  -- complete as far as possible
  db <- liftIO $ newIORef emptyProgDB
  irepl db i
  where
    irepl :: IOE :> es => IORef ProgDB -> Eff (Repl : es) a -> Eff es a
    irepl db = interpretIO \case
      ReadRepl    -> do
        m <- readlineMaybe "inet"
        case m of
          Nothing ->
            do
              _ <- exitSuccess
              return ""
          Just c -> return c
        
      WriteRepl s -> putFmtLn s
      ExitRepl -> exitSuccess
      WriteDB newdb -> writeIORef db newdb
      GetDB -> do
        db <- (readIORef db)
        return db
