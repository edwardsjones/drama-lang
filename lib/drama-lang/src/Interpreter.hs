{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import qualified Data.Map.Strict as M
import System.Random 
import Control.Monad.State
import Control.Lens
import Types
import Debug.Trace

data IState
    = IState
        { _isGlobalEnv :: GlobalEnv
        , _isLocalEnv :: LocalEnv
        }

    deriving (Eq, Show)

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
    = let (_, is') = flip runState is $
            do  instantiate inst
                scheduler
      in _isGlobalEnv is'
    where
        genv = initialEnv bs
        is = IState { _isGlobalEnv = genv, _isLocalEnv = undefined }


--neads to evaluate either a message, or prereceive
--will then call schedule again with new genv
--works out next actor by getting ids, getting ready ones, and then picking one
scheduler :: State IState ()
scheduler
    = do 
        is <- get
        let genv = _isGlobalEnv is
            allIds = M.keys (_geActorInstances genv)
        ready <- getReady allIds
        if null ready 
            then return ()
            else do 
                let toBeEval = head ready
                actor <- lookupActorInstance toBeEval
                let readyToReceive = _aiCanReceive actor
                    actorId = _aiId actor
                    Behaviour _ _ pr rec = _aiBehaviour actor
                    lenv = _aiEnv actor
                if readyToReceive
                    then do
                        v <- receive actor rec
                        is <- get
                        newActor <- lookupActorInstance actorId 
                        let newActor' = newActor { _aiEnv = _isLocalEnv is }
                            genv = _isGlobalEnv is
                            genv' = genv { _geActorInstances = M.update (replaceActor newActor) actorId (_geActorInstances genv) }
                            is' = is { _isGlobalEnv = genv' }
                        put is'
                        scheduler
                    else do 
                        v <- preReceive actorId pr
                        is <- get
                        newActor <- lookupActorInstance actorId 
                        let newActor' = newActor { _aiEnv = _isLocalEnv is }
                            genv = _isGlobalEnv is
                            genv' = genv { _geActorInstances = M.update (replaceActor newActor) actorId (_geActorInstances genv) }
                            is' = is { _isGlobalEnv = genv' }
                        put is'
                        scheduler

--preReceive :: ActorId -> PreReceive -> GlobalEnv -> LocalEnv -> (Value, GlobalEnv, LocalEnv)
preReceive :: ActorId -> PreReceive -> State IState Value
preReceive aid pr 
    = do
        v <- evalExp aid pr
        actor <- lookupActorInstance aid
        is <- get
        let newActor = actor { _aiCanReceive = True }
            genv = _isGlobalEnv is
            genv' = genv { _geActorInstances = M.update (replaceActor newActor) (_aiId actor) (_geActorInstances genv) }
            is' = is { _isGlobalEnv = genv' }
        put is'
        return v

replaceActor :: ActorInstance -> ActorInstance -> Maybe ActorInstance
replaceActor newActor oldActor = if oldActor == oldActor then Just newActor else Nothing

receive :: ActorInstance -> [Receive] -> State IState Value
receive ai rec 
    = do
        let (aps : msgs) = _aiInbox ai
            ai' = ai { _aiInbox = msgs }
        is <- get
        let genv = _isGlobalEnv is
            genv' = genv { _geActorInstances = M.update (replaceActor ai') (_aiId ai) (_geActorInstances genv) }
            (fps, handling) = matchArity rec aps
            bindings = M.fromList (zip fps aps)
            lenv = _isLocalEnv is
            lenv' = lenv { _leBindings = M.union (_leBindings lenv) bindings }
            is' = is { _isGlobalEnv = genv', _isLocalEnv = lenv' }
        put is'
        v <- evalExp (_aiId ai) handling 
        
        is2 <- get
        let lenv2 = _isLocalEnv is2
            lenv2' = lenv2 { _leBindings = _leBindings lenv }
            is2' = is2 { _isLocalEnv = lenv2' }
        put is2'

        return v

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

instantiate :: Instantiation -> State IState ActorId
instantiate (Instantiation name aps)
    = do
        is <- get
        let genv = _isGlobalEnv is
            lenv = (LocalEnv { _leBindings = M.fromList [], _leConsole = [] })
            is' = is { _isGlobalEnv = genv, _isLocalEnv = lenv }
        put is'
        ActorV firstId <- evalExp 0 (CreateE name aps) 
        return firstId

getReady :: [ActorId] -> State IState [ActorId]
getReady [] = return []
getReady (aid : aids)
    = do
        actorInst <- lookupActorInstance aid
        let ready = not (_aiCanReceive actorInst && null (_aiInbox actorInst))
        readyAids <- getReady aids
        return (if ready then aid : readyAids else readyAids)

--evalExp :: ActorId -> Exp -> GlobalEnv -> LocalEnv -> (Value, GlobalEnv, LocalEnv)
evalExp :: ActorId -> Exp -> State IState Value
evalExp _ UnitE
    = return UnitV

evalExp aid SelfE 
    = return (ActorV aid)

evalExp _ (NumberE x) 
    = return (NumberV x)

evalExp _ (VarE name)
    = lookupName name

evalExp selfId (SendE name aps)
    = do
        msg <- evalExps selfId aps
        ActorV otherId <- lookupName name
        otherInst <- lookupActorInstance otherId
        let newInbox = (_aiInbox otherInst) ++ [msg]
            otherInst' = otherInst { _aiInbox = newInbox }
        is <- get
        let genv = (_isGlobalEnv is)
            genv' = genv { _geActorInstances = M.insert otherId otherInst' (_geActorInstances genv) }
            is' = is { _isGlobalEnv = genv' }
        put is'
        return UnitV

evalExp aid (LetE name exp1 exp2)
    = do
        v <- evalExp aid exp1
        is <- get
        let lenv = _isLocalEnv is
            lenv' = lenv { _leBindings = M.insert name v (_leBindings lenv) }
            is' = is { _isLocalEnv = lenv' }
        put is'
        evalExp aid exp2

evalExp aid (CreateE name aps)
    = do 
        behaviour@(Behaviour _ fps _ _) <- lookupBehaviour name
        is <- get
        let genv = _isGlobalEnv is
            newId = _geNextAvailableActor genv
            genv' = genv { _geNextAvailableActor = newId + 1 }
            is' = is { _isGlobalEnv = genv' }
        put is'
        vs <- evalExps aid aps 
        let newLocalEnv 
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
        is2 <- get
        let genv2 = _isGlobalEnv is2
            genv2' = genv2 { _geActorInstances = M.insert newId newActor (_geActorInstances genv2) }
            is2' = is2 { _isGlobalEnv = genv2' }
        put is2'
        return (ActorV newId)

evalExp aid (PrintE s e)
    = do
        is <- get
        let lenv = _isLocalEnv is
            lenv' = lenv { _leConsole = _leConsole lenv ++ [s] }
        put (is { _isLocalEnv = lenv' })
        evalExp aid e

evalExps :: ActorId -> [Exp] -> State IState [Value]
evalExps _ [] 
    = return []

evalExps aid (e : es) 
    = do
        v <- evalExp aid e
        vs <- evalExps aid es
        return (v : vs)
        
lookupName :: Name -> State IState Value
lookupName name 
    = do 
        is <- get
        let lenv = _isLocalEnv is
        case M.lookup name (_leBindings lenv) of 
            Just v  -> return v
            Nothing -> error "lookupName: unbound variable"

lookupActorInstance :: ActorId -> State IState ActorInstance
lookupActorInstance aid 
    = do 
        is <- get
        let genv = _isGlobalEnv is
        case M.lookup aid (_geActorInstances genv) of 
            Just i  -> return i
            Nothing -> error "lookupActorInstance: invalid id"

lookupBehaviour :: Name -> State IState Behaviour
lookupBehaviour name 
    = do 
        is <- get
        let genv = _isGlobalEnv is
        case M.lookup name (_geBehaviours genv) of
            Just b  -> return b
            Nothing -> error "lookupBehaviour: unbound behaviour"
