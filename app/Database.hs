module Database where
import Ast
import qualified Data.HashMap.Strict as H
import Data.Text (Text)

  
-- maps a name to an agent
-- can't map name to name here - b/c we handle those differently
type ConsDB = H.HashMap Text Agent
type RulesDB = H.HashMap (Text, Text) InteractionRule
  
data ProgDB = ProgDB {
  localconsDB :: ConsDB,
  consDB :: ConsDB,
  rules :: RulesDB
              }

emptyProgDB :: ProgDB
emptyProgDB = ProgDB {
  localconsDB = H.empty
  , consDB = H.empty
  , rules = H.empty
                     }

