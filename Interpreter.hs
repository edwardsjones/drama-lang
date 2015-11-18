{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import qualified Data.Map.Strict as M
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
--scheduler picks actor with mail
--receive binds params and evaluates expression
--new genv, repeat

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

{-

type ActorId
    = Int deriving (Ord)

data LocalEnv
    = [(Name, Int)]

data Env
    = Env
        { _eNextAvailableActor :: ActorId
        , _eNextScheduledActor :: ActorId
        , _eActorInstances :: Map ActorId ActorInstance
        , _eLocalEnvironments :: Map ActorId LocalEnv
        , _eBehaviours :: [Behaviour]
        } deriving (Show)
makeLenses ''Env

data ActorInstance
    = ActorInstance
        { _aiId :: ActorId
        , _aiInbox :: [Message]
        , _aiBehaviour :: (Behaviour, ActualParams)
        } deriving (Show)
makeLenses ''ActorInstance

data Exp a where
    UnitE :: Exp ()
    SelfE :: Exp Int
    IntE :: Int -> Exp Int
    VarE :: Name -> Exp a
    SendE :: Int -> ActualParams -> Exp () 
    LetE :: Exp Name -> Exp Name -> Exp a -> Exp ()
    CreateE :: Name -> ActualParams -> Exp ActorId

--needs work..
eval :: Exp a -> Env -> (a, Env)
eval (UnitE) env = ((), env)
eval (SelfE) env = ((view _eNextScheduledActor), Env)
eval (IntE n) env = (n, env)
eval (VarE v) env = eval (lookup v env) env
eval (SendE e1 e2) env = ((send (eval e1 env) (eval e2 env)), env)
eval (LetE name e1 e2) env = eval e2 (addBinding name (eval e1 env) env)
eval (CreateE e1 e2) env = create (lookup e1 env) (eval e2 env)

--binds the list of behaviours to the global environment
initialEnv :: Program -> Env
initialEnv (Program [] _) = error "no behaviours defined"
initialEnv (Program behaviourList _) =  Env 
                                            { _eNextAvailableActor = ActorId 0
                                            , _eNextScheduledActor = ActorId 0
                                            , _eActorInstances = Map.empty
                                            , _eLocalEnvironments = Map.empty
                                            , _eBehaviours = behaviourList
                                            }

--creates first actor, changes env accordingly and calls step
instantiate :: Program -> Env -> (ActorInstance, Env)
instantiate (Program _ (Instantiation name params)) env =   let actor = ActorInstance 
                                                                            { _aiId = view _eNextAvailableActor
                                                                            , _aiInbox = []
                                                                            , _aiBehaviour = ((behaviourLookup name (view _eBehaviours)), params)
                                                                            } 
                                                                in step (actor, (set _eActorInstances

step :: ActorInstance -> Env -> (ActorInstance, Env)
--takes an actor instance and an environment,
--evaluates some code (ie receives a message or evals prereceive statements)
--returns the new changed actor state and the global environment
--gets next actor instance via scheduler, and recurses untill no more actors

scheduler :: [ActorInstance] -> Maybe ActorInstance
--looks at the list of actors and provides the next one to evaluate in
--returns None when execution is complete

--takes an actorId, sends a message, returns nothing
send :: ActorId -> Message -> ()
send id msg =   let actor = Map.lookup id (view _eActorInstances)
                    in --set actors inbox to current inbox with message on the end..

lookup :: Name -> Env -> Int
--have to use lenses to extract localenv

addBinding :: Name -> Int -> Env -> Env
--have to use lenses to extract localenv

--get behaviour, create new actor record and chang env accordingly
create :: Name -> ActualParams -> Env -> (ActorId, Env)
create name params env =    let actor = ActorInstance 
                                            { _aiId = view _eNextAvailableActor
                                            , _aiInbox = []
                                            , _aiBehvaiour = (behaviourLookup name (view _eBehaviours), params)
                                            }
                                in (actor, (set _eActorInstances --how to manipulate map?

addBinding :: Name -> a -> Env -> Env
addBinding name e env   = (name, e) : env

create :: Behaviour -> ActualParams -> Env -> [Actor]
create beh params env   = let act = Actor { address = newAddress, behaviour = beh, params = params, inbox = [] } in
                            let actorList = (lookup (Name "@actors") env) in
                                act : actorList

data Actor = Actor  { address :: Int
                    , behaviour :: Behaviour
                    , params :: ActualParams
                    , inbox :: [Message]
                    } deriving (Show, Eq) 

bindBehaviours :: [Behaviour] -> Env
bindBehaviours []   = []
bindBehaviours [(n fp pr rs)]    = [(n, (fp, pr, rs))]
bindBehaviours (n fp pr rs) : xs = (n, (fp, pr, rs)) : (bindBehaviours xs)

--add clauses to look for behaviours and actors first
lookup :: Name -> Env -> Exp a 
lookup n []         = error "no variable with that name"
lookup n (x,y):xs   = if n == x then y else lookup n xs

--possibly not needed
behaviourLookup :: Name -> BehaviourEnv -> --closure 
behaviourLookup n []    = error "no behaviour with that name"
behaviourLookup n (nm, (fp, pr, rs)) : xs   = if n == nm then (fp, pr, rs) else behaviourLookup n xs

send :: Address -> Env -> Message -> ()
send _ [] _ = error "no actor with that address"
send adr (Actor {address=x}):xs mes  = if x == adr then --put mes in actor's inbox else send adr xs mes
-}
