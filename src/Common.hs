module Common where

import           Data.Set
import           Data.Map

-- Identificadores
type Name = String
type AttrName = String
type Args = [Name]

-- Valores
data Value = StringVal String
           | IntVal Int
           | DoubleVal Double
           | AttrVal Name AttrName
           deriving (Eq, Ord)

-- Predicados
data Cond = Eq Value Value
          | NEq Value Value
          | Lt Value Value
          | Gt Value Value
          | LtE Value Value
          | GtE Value Value
          | And Cond Cond
          | Or Cond Cond
          | Not Cond

-- Una relación puede ser:
data Relation = Table Name                -- el nombre de una tabla cargada
              | Op Name Args              -- el nombre de una operación definido por el
                                          -- usuario, con sus argumentos
              | Sel Cond Relation         -- la operación de selección sobre una relación
              | Proj Attributes Relation  -- la operación de proyección sobre una relación
              | Rename Name Relation      -- la operación de renombramiento sobre una relación
              | Union Relation Relation   -- la unión de dos relaciones
              | Diff Relation Relation    -- la diferencia de dos relaciones
              | Prod Relation Relation    -- la producto cartesiano de dos relacione

-- Un statement puede ser:
data Stmt = DefRel Name Relation      -- la asignación de una relación a un nombre
          | DefTable Name Table       -- la asignación de una tabla a un nombre
          | DefOp Name Args Relation  -- la asignación de una operación a un nombre
          | Eval Relation             -- la evaluación de una relación

-- Tablas
type Attributes = [Value]
data Type = Str | Num deriving Eq
type Row = [Value]

data Table = T Attributes [Type] (Set Row)

-- Entorno de tablas y relaciones con sus respectivos identificadores
type Operator = (Relation, Args)
type Env = Map Name (Either Table Operator)
