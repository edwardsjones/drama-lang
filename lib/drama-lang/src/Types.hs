{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where 

import Data.Aeson
import GHC.Generics

data Program
    = Program 
        { progBehaviours :: [Behaviour] 
        , progInstantiation :: Instantiation
        }
    deriving (Generic, ToJSON, FromJSON, Eq, Show)

data Behaviour
    = Behaviour 
        { behaviourName :: Name 
        , behaviourFps :: FormalParams 
        , behaviourPr :: PreReceive 
        , behaviourRec :: [Receive]
        }
    deriving (Generic, ToJSON, FromJSON, Eq, Show)

type FormalParams
    = [FormalParam]

type FormalParam
    = Name

type PreReceive
    = Exp

type Receive
    = (Pat, Exp)

type Pat
    = FormalParams

type ActualParams
    = [ActualParam]

type ActualParam
    = Exp

type Name
    = String

data Instantiation
    = Instantiation 
        { instantiationName :: Name 
        , instantiationAps :: ActualParams
        }
    deriving (Generic, ToJSON, FromJSON, Eq, Show)

data Exp 
    = UnitE 
    | SelfE
    | BoolE String
    | NumberE Int
    | StringE String
    | VarE Name
    | ArithmeticE Exp Exp String
    | SendE Name ActualParams
    | LetE Name Exp Exp
    | CreateE Name ActualParams
    | PrintE String Exp
    | IfE Exp Exp Exp 
    | EqualityE Exp Exp String
    deriving (Generic, ToJSON, FromJSON, Eq, Show)
