{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Map.Strict as Map
import Control.Lens

data Program
    = Program [Behaviour] Instantiation

data Behaviour
    = Behaviour Name FormalParams PreReceive [Receive]

data FormalParams
    = [FormalParam]

type FormalParam
    = Name

type PreReceive
    = Exp

data Receive
    = Receive [Pat] Exp

data Pat
    = VarP Name

data ActualParams
    = [ActualParam]

type ActualParam
    = Exp

type Name
    = String

data Instantiation
    = Instantiation Name ActualParams

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

{-
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
