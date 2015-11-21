module Types where 

data Program
    = Program [Behaviour] Instantiation
    deriving (Eq, Show)

data Behaviour
    = Behaviour Name FormalParams PreReceive [Receive]
    deriving (Eq, Show)

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
    = Instantiation Name ActualParams
    deriving (Eq, Show)

data Exp 
    = UnitE 
    | SelfE
    | NumberE Int
    | VarE Name
    | SendE Name ActualParams
    | LetE Name Exp Exp
    | CreateE Name ActualParams
    | PrintE String Exp
    deriving (Eq, Show)
