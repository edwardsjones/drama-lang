{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    
    $white+                         ;
    create                          { \s -> CreateTk }
    behaviour                       { \s -> BehaviourTk }
    print                           { \s -> PrintTk }
    address                         { \s -> AddressTk }
    send                            { \s -> SendTk }
    receive                         { \s -> ReceiveTk }
    done                            { \s -> DoneTk }
    let                             { \s -> LetTk }
    in                              { \s -> InTk }
    self                            { \s -> SelfTk }
    $digit+                         { \s -> IntTk (read s) }
    \=                              { \s -> EqualsTk } 
    $alpha [$alpha $digit \_ \']*   { \s -> IdentifierTk s }
    \(                              { \s -> OpenParTk }
    \)                              { \s -> CloseParTk }
    \,                              { \s -> CommaTk }
    \-\>                            { \s -> HandleTk }
    \{                              { \s -> OpenBraceTk }
    \}                              { \s -> CloseBraceTk }
    
{
data Token =
    CreateTk            |
    BehaviourTk         |
    PrintTk             |
    AddressTk           |
    SendTk              |
    ReceiveTk           |
    DoneTk              |
    LetTk               |
    InTk                |
    SelfTk              |
    IntTk Int           | 
    EqualsTk            |
    IdentifierTk String |
    OpenParTk           |
    CloseParTk          |
    CommaTk             |
    HandleTk            |
    OpenBraceTk         |
    CloseBraceTk  
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
