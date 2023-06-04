module Ast where
import Data.Text (Text)

-- TODO add lens .. and use it


data Agent = Agent {
  aid :: Text,
  oport :: Maybe Term,
  port ::  [Term]
  }  deriving Show
newtype Name = Name Text  deriving Show

-- instance Show Name where
--   show (Name a) = a

-- instance Show Agent where
--   show (Agent aid Nothing _) = aid
--   show (Agent aid (Just o) ports) = aid <> "(" <> show o <> showPorts ports <> ")"
--     where
--       showPorts :: [Term] -> Text
--       showPorts [] = ""
--       showPorts (p:tl) = foldr  (\val ac -> ac <> "," <> show val) ("," <> show p) tl

-- instance Show Term where
--   show (N t) = show t
--   show (A t) = show t

data Term =
  N Name
  | A Agent  deriving Show

data Connection = Conn Term Term deriving Show
-- instance Show Connection where
--   show (Conn t1 t2) = show t1 <> "~" <> show t2


data InteractionRule = IRule {
  r1 :: RuleAgent,
  r2 :: RuleAgent,
  cons :: [Connection]
                             } deriving Show
type RuleAgent = Agent


data Expr =
  ATerm Term
  | ACon Connection
  | ARule InteractionRule deriving Show

type Ast = [Expr] -- an Ast is a list of expressions
-- instance Show InteractionRule where
--   show (IRule r1 r2 c) =
--     show r1 <> "><" <> show r2 <> "=>" <> showC c
--     where
--       showC [] = ""
--       showC (c:cs) = foldr  (\val ac -> ac <> "," <> show val) ("," <> show c) cs


