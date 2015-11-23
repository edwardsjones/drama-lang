{
module Happy where
import Tokens
import Types
}

%name parseDrama
%tokentype { Token }
%error { parseError }

%token
    create      { CreateTk }
    behaviour   { BehaviourTk }
    send        { SendTk }
    receive     { ReceiveTk }
    done        { DoneTk }
    let         { LetTk }
    in          { InTk }
    self        { SelfTk }
    int         { IntTk $$ }
    identifier  { IdentifierTk $$ }
    '='         { EqualsTk }
    '('         { OpenParTk }
    ')'         { CloseParTk }
    '{'         { OpenBraceTk }
    '}'         { CloseBraceTk }
    ','         { CommaTk }
    '->'        { HandleTk }

%right in
%left '->'
%left '='
%%

Program         : BehaviourList Instantiation               {}

BehaviourList   : Behaviour                                 { [$1] }
                | BehaviourList Behaviour                   { $2 : $1 }

Behaviour       : behaviour identifier '(' FormalParams ')'
                  '{' Exp Receive '}'                       { Behaviour $2 $4 $7 $8 }

Receive         : receive Handling done                     { $2 }

Handling        : '(' MsgFP ')' '->' Exp                    { [($2, $5)] }
                | Handling '(' MsgFP ')' '->' Exp           { ($3, $6) : $1 }

MsgFP           : {-- empty --}                             { [] }
                | FormalParam                               { [$1] }
                | MsgFP ',' FormalParam                     { $3 : $1 }

MsgAP           : {-- empty --}                             { [] }
                | ActualParam                               { [$1] }
                | MsgAP ',' ActualParam                     { $3 : $1 }

Exp             : '(' ')'                                   { UnitE }
                | self                                      { SelfE }
                | identifier                                { VarE $1 }
                | int                                       { NumberE $1 }
                | send identifier '(' MsgAP ')'             { SendE $2 $4 }
                | let identifier '=' Exp in Exp             { LetE $2 $4 $6 }
                | create identifier '(' ActualParams ')'    { CreateE $2 $4 }

Instantiation   : create identifier '(' ActualParams ')'    { Instantiation $2 $4 }

FormalParams    : {-- empty --}                             { [] }
                | FormalParams FormalParam                  { $2 : $1 }

ActualParams    : {-- empty --}                             { [] }
                | ActualParams ActualParam                  { $2 : $1 }

FormalParam     : identifier                                { $1 }

ActualParam     : Exp                                       { $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error."

}
