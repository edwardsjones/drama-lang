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
    head                            { \s -> ListOpTk s }
    tail                            { \s -> ListOpTk s }
    init                            { \s -> ListOpTk s }
    last                            { \s -> ListOpTk s }
    length                          { \s -> ListOpTk s }
    encrypt                         { \s -> EncryptTk }
    decrypt                         { \s -> DecryptTk }
    \+                              { \s -> ArithmeticTk s }
    \-                              { \s -> ArithmeticTk s }
    \/                              { \s -> ArithmeticTk s }
    \*                              { \s -> ArithmeticTk s }
    true                            { \s -> BoolTk s }
    false                           { \s -> BoolTk s }
    if                              { \s -> IfTk }
    then                            { \s -> ThenTk }
    else                            { \s -> ElseTk }
    \=\=                            { \s -> EqualityTk s }
    \!\=                            { \s -> EqualityTk s }
    \>                              { \s -> EqualityTk s }
    \<                              { \s -> EqualityTk s }
    \>=                             { \s -> EqualityTk s }
    \<=                             { \s -> EqualityTk s }
    $digit+                         { \s -> IntTk (read s) }
    \=                              { \s -> EqualsTk } 
    $alpha [$alpha $digit \_ \']*   { \s -> IdentifierTk s }
    \" [$white $digit $alpha \, \. \! \£ \$ \% \^ \& \* \( \) \- \_ \? \> \< \@ \~]* \"
                                    { \s -> StringTk (tail (init s)) }
    \(                              { \s -> OpenParTk }
    \)                              { \s -> CloseParTk }
    \[                              { \s -> OpenListTk }
    \]                              { \s -> CloseListTk }
    \:                              { \s -> ConsTk }
    \,                              { \s -> CommaTk }
    \-\>                            { \s -> HandleTk }
    \{                              { \s -> OpenBraceTk }
    \}                              { \s -> CloseBraceTk }
    
{
data Token 
    = CreateTk            
    | BehaviourTk         
    | PrintTk             
    | AddressTk           
    | SendTk              
    | ReceiveTk           
    | DoneTk              
    | LetTk               
    | InTk                
    | SelfTk              
    | ListOpTk String
    | BoolTk String
    | IfTk                
    | ThenTk              
    | ElseTk              
    | EncryptTk
    | DecryptTk
    | EqualityTk String
    | ArithmeticTk String
    | IntTk Int           
    | EqualsTk            
    | IdentifierTk String 
    | StringTk String
    | OpenParTk           
    | CloseParTk          
    | OpenListTk
    | CloseListTk
    | ConsTk
    | CommaTk             
    | HandleTk            
    | OpenBraceTk         
    | CloseBraceTk  
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
