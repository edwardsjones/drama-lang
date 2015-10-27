{
module Happy where
import Tokens
}

%name parseDrama
%tokentype { Token }
%error { parseError }

%token
    create      { Create }
    behaviour   { Behaviour }
    send        { Send }
    receive     { Receive }
    done        { Done }
    let         { Let }
    in          { In }
    int         { Int $$ }
    identifier  { Identifier $$ }
    '='         { Equals }
    '('         { OpenPar }
    ')'         { ClosePar }
    ','         { Comma }
    '->'        { Handle }

%right in
%left '->'
%%

Program         : BehaviourList Instantiation       {}

BehaviourList   : Behaviour                         { [$1] }
                | BehaviourList Behaviour           { $2 : $1 }

Behaviour       : behaviour identifier FormalParam 
                  Exp Receive                       { Behaviour $2 $3 $4 $5 }

Receive         : receive Handling done             { $2 }

Handling        : '(' Msg ')' '->' Exp              { [(Receive $2 $5)] }
                | Handling '(' Msg ')' '->' Exp     { (Receive $3 $6) : $1 }

Msg             : {-- empty --}                     { [] }
                | identifier                        { [$1] }
                | Msg ',' identifier                { $3 : $1 }

Exp             : '(' ')'                           { () }
                | identifier                        { VarE $1 }
                | int                               { NumberE $1 }
                | send identifier '(' Msg ')'       { SendE $2 $4 }
                | let identifier '=' Exp in Exp     { LetE $2 $4 $6 }
                | create identifier ActualParam     { CreateE $2 $3 }

Instantiation   : create identifier ActualParam     { Instantiation $2 $3 }

FormalParam     : {-- empty --}                     { [] }
                | FormalParam identifier            { $2 : $1 }

ActualParam     : {-- empty --}                     { [] }
                | ActualParam Value                 { $2 : $1 }

Value           : int                               { $1 }
                | identifier                        { $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error."

data Program
    = Program [Behaviour] Instantiation

data Behaviour
    = Behaviour Name [FormalParam] PreReceive [Receive]

type FormalParam
    = Name

type PreReceive
    = Exp

data Receive
    = Receive [Pat] Exp

data Pat
    = VarP Name

data Exp
    = UnitE
    | VarE Name
    | NumberE Int
    | SendE Name [ActualParam]
    | LetE Name Exp Exp
    | CreateE Name [ActualParam]

type ActualParam
    = Exp

type Name
    = String

data Instantiation
    = Instantiation Name [ActualParam]

}
