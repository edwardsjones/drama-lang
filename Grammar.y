{
module Grammar where
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

Program     : Exp                                   { [$1] }
            | Program Exp                           { $2 : $1 }

Exp         : let identifier '=' Exp in Exp         { Let $2 $4 $6 }
            | create identifier Args                { Create $2 $3 }
            | behaviour identifier Args Exp         { Behaviour $2 $3 $4 }
            | send Value '(' Message ')'            { Send $2 $4 } 
            | receive HandleExp                     { Receive $2 }

Args        : Value                                 { [$1] }
            | Args Value                            { $2 : $1 }

HandleExp   : HandleExp '(' Message ')' '->' Exp    { ($3, $6) : $1 }
            | done                                  { [] }

Message     : Value                                 { [$1] }
            | Message ',' Value                     { $2 : $1 }

Value       : identifier                            { Identifier $1 }
            | int                                   { Int $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Program    = [Exp]
                deriving(Show)

data Exp        = Let String Exp Exp
                | Create String Args 
                | Behaviour String Args Exp
                | Send Value Message
                | Receive HandleExp
                deriving(Show)

data Args       = [Value]
                deriving(Show)

data HandleExp  = [(Message, Exp)]
                deriving(Show)

data Message    = [Value]
                deriving(Show)

data Value      = Int Int
                | Identifier String
                deriving(Show)

}
