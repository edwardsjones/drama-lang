{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import qualified Data.Map.Strict as M
import System.Random 
import Control.Lens
import Types

type ActorId
    = Int

data GlobalEnv 
    = GlobalEnv 
        { _geNextAvailableActor :: ActorId
        , _geBehaviours :: M.Map Name Behaviour
        , _geActorInstances :: M.Map ActorId ActorInstance
        } 
    
    deriving (Eq, Show)

data ActorInstance
    = ActorInstance
        { _aiId :: ActorId
        , _aiInbox :: [Message]
        , _aiBehaviour :: Behaviour
        , _aiEnv :: LocalEnv
        , _aiCanReceive :: Bool
        }

    deriving (Eq, Show)

data LocalEnv
    = LocalEnv
        { _leBindings :: M.Map Name Value
        , _leConsole :: [String]
        }

    deriving (Eq, Show)

data Value
    = UnitV
    | NumberV Int
    | ActorV ActorId
    deriving (Eq, Show)

type Message
    = [Value]

--createE for instantiation
--pass to scheduler
--scheduler picks actor with mail or who hasn't executed prereceive
--receive binds params and evaluates expression, returning new genv and lenv
--bind lenv, call scheduler again with new genv
--repeat untill no mail
evalProgram :: Program -> GlobalEnv
evalProgram (Program bs inst)
    = scheduler genv'
    where
        genv = initialEnv bs
        (aid, genv') = instantiate inst genv 

--neads to evaluate either a message, or prereceive
--will then call schedule again with new genv
--works out next actor by getting ids, getting ready ones, and then picking one
scheduler :: GlobalEnv -> GlobalEnv
scheduler genv
    = if null ready 
        then genv
        else let toBeEval = head ready 
                 actor = (lookupActorInstance toBeEval genv)
                 readyToReceive = (_aiCanReceive actor) 
                 actorId = (_aiId actor)
                 (Behaviour _ _ pr rec) = (_aiBehaviour actor)
                 lenv = (_aiEnv actor) in
            if readyToReceive 
                then let (v, genv', lenv') = receive actor genv lenv rec 
                         newActor = (lookupActorInstance actorId genv') { _aiEnv = lenv' } 
                         genv'' = genv' { _geActorInstances = (M.update (replaceActor newActor) actorId (_geActorInstances genv')) } in
                    scheduler genv''
                else let (v, genv', lenv') = preReceive actorId pr genv lenv 
                         newActor = (lookupActorInstance actorId genv') { _aiEnv = lenv' } 
                         genv'' = genv' { _geActorInstances = (M.update (replaceActor newActor) actorId (_geActorInstances genv')) } in
                    scheduler genv''
    where 
        allIds = M.keys (_geActorInstances genv)
        ready = getReady allIds genv

preReceive :: ActorId -> PreReceive -> GlobalEnv -> LocalEnv -> (Value, GlobalEnv, LocalEnv)
preReceive aid pr genv lenv 
    = (v, genv'', lenv')
    where
        (v, genv', lenv') = evalExp aid pr genv lenv
        actor = lookupActorInstance aid genv'
        newActor = actor { _aiCanReceive = True }
        genv'' = genv' { _geActorInstances = (M.update (replaceActor newActor) (_aiId actor) (_geActorInstances genv')) }

replaceActor :: ActorInstance -> ActorInstance -> Maybe ActorInstance
replaceActor newActor oldActor = if oldActor == oldActor then Just newActor else Nothing

receive :: ActorInstance -> GlobalEnv -> LocalEnv -> [Receive] -> (Value, GlobalEnv, LocalEnv)
receive ai genv lenv rec 
    = (v, genv'', lenv''')
    where
        --remove msg, and populate genv with updated actor
        actualMsgParams = head (_aiInbox ai)
        ai' = ai { _aiInbox = tail (_aiInbox ai) }
        genv' = genv { _geActorInstances = (M.update (replaceActor ai') (_aiId ai) (_geActorInstances genv)) }

        --match on arity and bind params
        matching = matchArity rec actualMsgParams
        formalMsgParams = fst matching
        handlingExp = snd matching
        bindings = M.fromList (zip formalMsgParams actualMsgParams)
        lenv' = lenv { _leBindings = M.union (_leBindings lenv) bindings }
    
        --eval exp and unbind params, but keep debug strings
        (v, genv'', lenv'') = evalExp (_aiId ai) handlingExp genv' lenv'
        lenv''' = lenv' { _leConsole = (_leConsole lenv'') }
        

matchArity :: [Receive] -> Message -> Receive
matchArity [] _ = error "matchArity: no matching cases for message"
matchArity (r : rs) msg = if length r == length msg then r else matchArity rs msg

initialEnv :: [Behaviour] -> GlobalEnv
initialEnv [] = error "initalEnv: no behaviours defined"
initialEnv bs
    = GlobalEnv
        { _geNextAvailableActor = 1
        , _geBehaviours = behaviourMap
        , _geActorInstances = M.fromList []
        } 
    where 
        behaviourNames = getNames bs
        behaviourMap = M.fromList (zip behaviourNames bs)

getNames :: [Behaviour] -> [Name]
getNames [] = []
getNames ((Behaviour name _ _ _) : bs) = name : getNames bs

instantiate :: Instantiation -> GlobalEnv -> (ActorId, GlobalEnv)
instantiate (Instantiation name aps) genv
    = (firstId, genv')
    where
        (ActorV firstId, genv', lenv) = evalExp 0 (CreateE name aps) genv (LocalEnv { _leBindings = M.fromList [], _leConsole = [] })

getReady :: [ActorId] -> GlobalEnv -> [ActorId]
getReady [] _ = []
getReady (aid : aids) genv
    = if ready then aid : getReady aids genv else getReady aids genv
    where 
        actorInst = lookupActorInstance aid genv
        ready = if (not (_aiCanReceive actorInst)) then True else not (null (_aiInbox actorInst))

pickRandom :: [a] -> IO a
pickRandom as = randomRIO (0, length as - 1) >>= return . (as !!)

evalExp :: ActorId -> Exp -> GlobalEnv -> LocalEnv -> (Value, GlobalEnv, LocalEnv)
evalExp _ UnitE genv lenv 
    = (UnitV, genv, lenv)

evalExp aid SelfE genv lenv
    = (ActorV aid, genv, lenv)

evalExp _ (NumberE x) genv lenv
    = (NumberV x, genv, lenv)

evalExp _ (VarE name) genv lenv
    = (lookupName name lenv, genv, lenv)

evalExp selfId (SendE name aps) genv lenv
    = (UnitV, genv'', lenv)
    where
        ActorV otherId = lookupName name lenv
        (msg, genv', lenv') = evalExps selfId aps genv lenv
        otherInst = lookupActorInstance otherId genv' 
        newInbox = (_aiInbox otherInst) ++ [msg]
        otherInst' = otherInst { _aiInbox = newInbox }
        genv'' = genv' { _geActorInstances = M.insert otherId otherInst' (_geActorInstances genv') }

evalExp aid (LetE name exp1 exp2) genv lenv
    = evalExp aid exp2 genv' lenv''
    where
        (v, genv', lenv') = evalExp aid exp1 genv lenv
        lenv'' = lenv' { _leBindings = M.insert name v (_leBindings lenv') }

evalExp aid (CreateE name aps) genv lenv
    = (ActorV newId, genv''', lenv')
    where
        behaviour@(Behaviour _ fps _ _) = lookupBehaviour name genv
        newId = _geNextAvailableActor genv
        genv' = genv { _geNextAvailableActor = newId + 1 }
        (vs, genv'', lenv') = evalExps aid aps genv' lenv
        newLocalEnv 
            = LocalEnv 
                { _leBindings = M.fromList (zip fps vs) 
                , _leConsole = []
                }
        newActor 
            = ActorInstance
                { _aiId = newId
                , _aiInbox = []
                , _aiBehaviour = behaviour
                , _aiEnv = newLocalEnv
                , _aiCanReceive = False
                }
        genv''' = genv'' { _geActorInstances = M.insert newId newActor (_geActorInstances genv'') }

evalExp aid (PrintE s e) genv lenv
    = evalExp aid e genv lenv'
    where
        lenv' = lenv { _leConsole = _leConsole lenv ++ [s] }

evalExps :: ActorId -> [Exp] -> GlobalEnv -> LocalEnv -> ([Value], GlobalEnv, LocalEnv)
evalExps _ [] genv lenv
    = ([], genv, lenv)
evalExps aid (e : es) genv lenv
    = (v : vs, genv'', lenv'') 
    where 
        (v, genv', lenv') = evalExp aid e genv lenv
        (vs, genv'', lenv'') = evalExps aid es genv' lenv'
        
lookupName :: Name -> LocalEnv -> Value
lookupName name lenv
    = case M.lookup name (_leBindings lenv) of 
        Just v  -> v
        Nothing -> error "lookupName: unbound variable"

lookupActorInstance :: ActorId -> GlobalEnv -> ActorInstance
lookupActorInstance aid genv
    = case M.lookup aid (_geActorInstances genv) of 
        Just i  -> i
        Nothing -> error "lookupActorInstance: invalid id"

lookupBehaviour :: Name -> GlobalEnv -> Behaviour
lookupBehaviour name genv
    = case M.lookup name (_geBehaviours genv) of
        Just b  -> b
        Nothing -> error "lookupBehaviour: unbound behaviour"
