{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    
    $white+         ;
    create                          { \s -> Create }
    behaviour                       { \s -> Behaviour }
    address                         { \s -> Address }
    send                            { \s -> Send }
    receive                         { \s -> Receive }
    done                            { \s -> Done }
    let                             { \s -> Let }
    in                              { \s -> In }
    $digit+                         { \s -> Int (read s) }
    \=                              { \s -> Equals } 
    $alpha [$alpha $digit \_ \']*   { \s -> Identifier s }
    \(                              { \s -> OpenPar }
    \)                              { \s -> ClosePar }
    \,                              { \s -> Comma }
    \-\>                            { \s -> Handle }
    
{
data Token =
    Create              |
    Behaviour           |
    Address             |
    Send                |
    Receive             |
    Done                |
    Let                 |
    In                  |
    Int Int             | 
    Equals              |
    Identifier String   |
    OpenPar             |
    ClosePar            |
    Comma               |
    Handle      
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
