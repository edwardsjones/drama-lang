{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main (main) where

import System.Environment ( getArgs ) 
import System.Exit ( exitWith )
import Test.Framework 
import Data.Map.Strict
import Control.Monad.State
import Control.Exception

import Types            as T
import Interpreter      as I
import Happy            as P
import Tokens           as L

test_values
    = do
        progStr <- readFile "tests/test1.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is =  getEndState ast
            genv = I._isGlobalEnv is
            actor = (I._geActorInstances genv) ! 1
            lenv = I._aiEnv actor
            bindings = I._leBindings lenv
        assertEqual I.UnitV (bindings ! "unit")
        assertEqual (I.ActorV 1) (bindings ! "address")
        assertEqual (I.BoolV True) (bindings ! "t")
        assertEqual (I.BoolV False) (bindings ! "f")
        assertEqual (I.StringV " ,.!£$%^&*()-_?><@~ hasdfj 123 AJS ") (bindings ! "str")
        assertEqual (I.StringV "") (bindings ! "empty_str")
        assertEqual (I.NumberV 32) (bindings ! "num")
        assertEqual (I.ListV [(I.NumberV 1), (I.NumberV 2), (I.NumberV 3)]) (bindings ! "xs")
        assertEqual (I.ListV []) (bindings ! "empty_xs")

test_lists
    = do
        progStr <- readFile "tests/test2.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            actor = (I._geActorInstances genv) ! 1
            lenv = I._aiEnv actor
            bindings = I._leBindings lenv
        assertEqual (I.ListV xs) (bindings ! "xs")
        assertEqual (I.NumberV 1) (bindings ! "hxs")
        assertEqual (I.ListV (tail xs)) (bindings ! "txs")
        assertEqual (I.NumberV 4) (bindings ! "lxs")
        assertEqual (I.ListV (init xs)) (bindings ! "ixs")
        assertEqual (I.NumberV 4) (bindings ! "len")
        assertEqual (I.ListV ((StringV "hi") : xs)) (bindings ! "built")
    where
        xs = [(I.NumberV 1), (I.NumberV 2), (I.NumberV 3), (I.NumberV 4)]

test_variableScope
    = do
        progStr <- readFile "tests/test3.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            actor = (I._geActorInstances genv) ! 2
            lenv = I._aiEnv actor
            console = I._leConsole lenv
        assertEqual "pass" (head console)

test_messagesSend
    = do
        progStr <- readFile "tests/test4.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            actor = (I._geActorInstances genv) ! 2
            lenv = I._aiEnv actor
            console = I._leConsole lenv
        assertEqual "received1" (console !! 0)
        assertEqual "received2" (console !! 1)
        assertEqual "received3" (console !! 2)
        assertEqual "received4" (console !! 3)
        assertEqual "received5" (console !! 4)
         
test_aidIncrementing
    = do
        progStr <- readFile "tests/test5.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            first = (I._geActorInstances genv) ! 1
            second = (I._geActorInstances genv) ! 2
            third = (I._geActorInstances genv) ! 3
            c1 = I._leConsole (I._aiEnv first)
            c2 = I._leConsole (I._aiEnv second)
            c3 = I._leConsole (I._aiEnv third)
        assertEqual "first" (head c1)
        assertEqual "second" (head c2)
        assertEqual "third" (head c3)
        assertEqual 3 (I._isCurrentAID is)

test_conditionals
    = do
        progStr <- readFile "tests/test6.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            false = (I._geActorInstances genv) ! 2
            truth = (I._geActorInstances genv) ! 3
            cf = I._leConsole (I._aiEnv false)
            ct = I._leConsole (I._aiEnv truth)
        assertEqual "pass" (head cf)
        assertEqual "pass" (head ct)

test_equalityOperators
    = do
        progStr <- readFile "tests/test7.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            eqActor = (I._geActorInstances genv) ! 2
            ineqActor = (I._geActorInstances genv) ! 3
            gtActor = (I._geActorInstances genv) ! 4
            gteActor = (I._geActorInstances genv) ! 5
            ltActor = (I._geActorInstances genv) ! 6
            lteActor = (I._geActorInstances genv) ! 7
            cEq = I._leConsole (I._aiEnv eqActor)
            cIneq = I._leConsole (I._aiEnv ineqActor)
            cGt = I._leConsole (I._aiEnv gtActor)
            cGte = I._leConsole (I._aiEnv gteActor)
            cLt = I._leConsole (I._aiEnv ltActor)
            cLte = I._leConsole (I._aiEnv lteActor)
        assertEqual "eq_pass" (head cEq)
        assertEqual "ineq_pass" (head cIneq)
        assertEqual "gt_pass" (head cGt)
        assertEqual "gte_pass" (head cGte)
        assertEqual "lt_pass" (head cLt)
        assertEqual "lte_pass" (head cLte)

test_receiveMatching
    = do
        progStr <- readFile "tests/test8.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            actor = (I._geActorInstances genv) ! 2
            con = I._leConsole (I._aiEnv actor)
        assertEqual "num_pass" (con !! 0)
        assertEqual "str_pass" (con !! 1)
        assertEqual "unit_pass" (con !! 2)
        assertEqual "arity_pass" (con !! 3)

test_arithmetic
    = do
        progStr <- readFile "tests/test9.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            actor = (I._geActorInstances genv) ! 1
            con = I._leConsole (I._aiEnv actor)
        assertEqual "arithmetic_pass" (head con)

test_encryption
    = do
        progStr <- readFile "tests/test10.dr"
        let ast = P.parseDrama $ L.alexScanTokens progStr
            is = getEndState ast
            genv = I._isGlobalEnv is
            actor = (I._geActorInstances genv) ! 1
            con = I._leConsole (I._aiEnv actor)
        assertEqual "pass" (head con) 

getEndState :: T.Program -> I.IState
getEndState (T.Program bs inst) 
    = let (_, is') = flip runState is $
            do  instantiate inst
                scheduler
      in is'
    where
        genv = I.initialEnv bs
        is = I.IState { I._isGlobalEnv = genv, I._isCurrentAID = 0 }

main = htfMain htf_thisModulesTests
