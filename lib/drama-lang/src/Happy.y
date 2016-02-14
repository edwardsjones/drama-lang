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
    print       { PrintTk }
    let         { LetTk }
    in          { InTk }
    self        { SelfTk }
    bools       { BoolTk $$ }
    if          { IfTk }
    then        { ThenTk }
    else        { ElseTk }
    arith_op    { ArithmeticTk $$ }
    eq          { EqualityTk $$ }
    int         { IntTk $$ }
    identifier  { IdentifierTk $$ }
    str         { StringTk $$ }
    '='         { EqualsTk }
    '('         { OpenParTk }
    ')'         { CloseParTk }
    '['         { OpenListTk }
    ']'         { CloseListTk }
    '{'         { OpenBraceTk }
    '}'         { CloseBraceTk }
    ','         { CommaTk }
    '->'        { HandleTk }

%right in
%left '->'
%left '='
%%

Program         : BehaviourList Instantiation               { Program $1 $2 }

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
                | ListExp                                   { ListE $1 }
                | bools                                     { BoolE $1 }
                | print identifier Exp                      { PrintE $2 $3 }
                | identifier                                { VarE $1 }
                | int                                       { NumberE $1 }
                | str                                       { StringE $1 }
                | Exp eq Exp                                { EqualityE $1 $3 $2 }
                | Exp arith_op Exp                          { ArithmeticE $1 $3 $2 }
                | send identifier '(' MsgAP ')'             { SendE $2 $4 }
                | let identifier '=' Exp in Exp             { LetE $2 $4 $6 }
                | create identifier '(' ActualParams ')'    { CreateE $2 $4 }
                | if '(' Exp ')' then '{' Exp '}' else '{'
                  Exp '}'                                   { IfE $3 $7 $11 }

ListExp         : '[' List ']'                              { $2 }

List            : {-- empty --}                             { [] }
                | Exp                                       { [$1] }
                | List ',' Exp                              { $3 : $1 }

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
