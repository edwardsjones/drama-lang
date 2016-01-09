{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Interpreter where

import qualified Data.Map.Strict as M
import System.Random 
import System.IO
import Control.Monad.State
import Control.Lens
import Types
import Debug.Trace

data IState
    = IState
        { _isGlobalEnv :: GlobalEnv
        , _isCurrentAID :: ActorId
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

$(makeLenses ''IState)
$(makeLenses ''GlobalEnv)
$(makeLenses ''LocalEnv)
$(makeLenses ''ActorInstance)

data Stepped s a
    = Stepped (s -> (SteppedResult s a, s))

data SteppedResult s a
    = Done a
    | Suspended (Stepped s a)

runStepped :: Stepped s a -> s -> (SteppedResult s a, s)
runStepped (Stepped f) s
    = f s

instance Functor (Stepped s) where
    fmap f (Stepped g)
        = Stepped $
            \s ->   let (r, s') = g s
                    in  (fmap f r, s')

instance Functor (SteppedResult s) where
  --fmap takes an a -> b, and a structure, and applies that function
    fmap f (Done a)
        = Done (f a)

    fmap f (Suspended k)
        = Suspended (fmap f k)

instance Applicative (Stepped s) where
    pure
        = return

    sf <*> sx
        = do
            f <- sf
            x <- sx
            return (f x)

instance Monad (Stepped s) where
    
    return x
        = Stepped $
            \s -> (Done x, s)

--(>>=) :: Stepped s a -> (a -> Stepped s b) -> Stepped s b
    ma >>= kamb
        = Stepped $

            \s -> case runStepped ma s of
                (Done x, s') -> runStepped (kamb x) s'
                (Suspended sa, s') -> (Suspended (sa >>= kamb), s')    

instance MonadState s (Stepped s) where
    get 
        = Stepped $ \s -> (Done s, s)

    put newState
        = Stepped $ \_ -> (Done (), newState)

class MonadState s m => MonadStepped s m where
    defineStep :: m a -> m a

instance Monad m => MonadStepped s (StateT s m) where
    defineStep 
        = id

instance MonadStepped s (Stepped s) where
  --defineStep :: Stepped s a -> Stepped s a
    defineStep m
        = Stepped $ \s -> (Suspended m, s)

step :: Stepped s a -> s -> (Maybe (Stepped s a), s)
step (Stepped f) s
    = case f s of
        (Done x, s') -> (Nothing, s')
        (Suspended k, s') -> (Just k, s')
    
isLocalEnv :: Lens' IState LocalEnv
isLocalEnv f is
    = fmap k (f lenv)
    where
        genv = _isGlobalEnv is
        aid = _isCurrentAID is
        instances = _geActorInstances genv
        actor = M.findWithDefault (error ("isLocalEnv error: no actor with id " ++ show aid)) aid instances
        lenv = _aiEnv actor
        k lenv' = is { _isGlobalEnv = genv { _geActorInstances = M.insert aid (actor { _aiEnv = lenv' }) instances } }

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
        is = IState { _isGlobalEnv = genv, _isCurrentAID = 0 }

stepProgram :: Program -> IO ()
stepProgram (Program bs inst)
    = let (maybeSusp, is') = step (instantiate inst >> scheduler) is
      in stepper maybeSusp is'
    where 
        genv = initialEnv bs
        is = IState { _isGlobalEnv = genv, _isCurrentAID = 0 }

--Pattern matches to see if execution is finished or not.
--If not, then crudely waits for input before carrying on
stepper :: Maybe (Stepped IState a)-> IState -> IO ()
stepper maybeSusp is 
    = case maybeSusp of
        Just susp -> let (nextSusp, is') = step susp is 
                     in do hSetBuffering stdout NoBuffering
                           prettyPrintGlobalEnv (_isGlobalEnv is')
                           input <- getLine 
                           stepper nextSusp is'
        Nothing   -> putStrLn "Done"

--Somewhat ugly pretty printer, for testing
prettyPrintGlobalEnv :: GlobalEnv -> IO ()
prettyPrintGlobalEnv genv
    = let nextActorString = "Next Available Actor ID: " ++ (show nextAvailableActor)
          actorStringList = M.foldl (\a -> \str -> a ++ str) "" (M.map formatInstance actorList)
      in do putStrLn "========================================"
            putStrLn nextActorString
            putStrLn actorStringList
    where
        nextAvailableActor = _geNextAvailableActor genv
        actorList = _geActorInstances genv
        

formatInstance :: ActorInstance -> String
formatInstance (ActorInstance id inbox (Behaviour name _ _ _) (LocalEnv bindings console) rec)
    = "\nActor ID: " ++ (show id) ++ "\nInbox: " ++ (show inbox) ++ "\nBehaviour: " ++  (show name) ++ "\nBindings: " ++ (show (M.toList bindings)) ++ "\nConsole: " ++ (show console) ++ "\nReady to receive: " ++ (show rec) ++ "\n"

--neads to evaluate either a message, or prereceive
--will then call schedule again with new genv
--works out next actor by getting ids, getting ready ones, and then picking one
scheduler :: MonadStepped IState m => m ()
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
                isCurrentAID .= actorId
                if readyToReceive
                    then do
                        v <- receive actor rec
                        newActor <- lookupActorInstance actorId 
                        isGlobalEnv . geActorInstances %= M.update (replaceActor newActor) actorId
                        scheduler
                    else do 
                        v <- preReceive actorId pr
                        newActor <- lookupActorInstance actorId 
                        isGlobalEnv . geActorInstances %= M.update (replaceActor newActor) actorId
                        scheduler

preReceive :: MonadStepped IState m => ActorId -> PreReceive -> m Value
preReceive aid pr 
    = defineStep $ 
        do
            v <- evalExp aid pr
            actor <- lookupActorInstance aid
            let newActor = actor { _aiCanReceive = True }
            isGlobalEnv . geActorInstances %= M.update (replaceActor newActor) (_aiId actor) 
            return v

replaceActor :: ActorInstance -> ActorInstance -> Maybe ActorInstance
replaceActor newActor oldActor = if oldActor == oldActor then Just newActor else Nothing

receive :: MonadStepped IState m => ActorInstance -> [Receive] -> m Value
receive ai rec 
    = defineStep $
        do
            -- get aps from msg, and take out that msg
            let (aps : msgs) = _aiInbox ai
                ai' = ai { _aiInbox = msgs }

            -- replace the actor with one which doesn't have that msg anymore
            isGlobalEnv . geActorInstances %= M.update (replaceActor ai') (_aiId ai)

            -- find which handler to use, and create new msg bindings
            let (fps, handling) = matchArity rec aps
                bindings = M.fromList (zip fps aps)
            -- get env without msg bindings, for after
            lenv <- use isLocalEnv
            -- add msg bindings to lenv
            isLocalEnv . leBindings %= M.union bindings

            -- eval expression of matched msg handler
            v <- evalExp (_aiId ai) handling 
            
            -- make sure to get rid of temporary msg bindings in actor's local env
            -- put original bindings back
            isLocalEnv . leBindings .= _leBindings lenv
            -- get current lenv, minus temp bindings
            lenv2 <- use isLocalEnv
            -- actor becomes one after execution, but without msg bindings
            let ai2 = ai' { _aiEnv = lenv2 }
            -- replace actor with new one
            isGlobalEnv . geActorInstances %= M.update (replaceActor ai2) (_aiId ai2)

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
        , _geActorInstances = M.empty
        } 
    where 
        behaviourNames = getNames bs
        behaviourMap = M.fromList (zip behaviourNames bs)

getNames :: [Behaviour] -> [Name]
getNames [] = []
getNames ((Behaviour name _ _ _) : bs) = name : getNames bs

instantiate :: MonadStepped IState m => Instantiation -> m ActorId
instantiate (Instantiation name aps)
    = do
        ActorV firstId <- evalExp 0 (CreateE name aps) 
        return firstId

getReady :: MonadStepped IState m => [ActorId] -> m [ActorId]
getReady [] = return []
getReady (aid : aids)
    = do
        actorInst <- lookupActorInstance aid
        let ready = not (_aiCanReceive actorInst && null (_aiInbox actorInst))
        readyAids <- getReady aids
        return (if ready then aid : readyAids else readyAids)

evalExp :: MonadStepped IState m => ActorId -> Exp -> m Value
evalExp _ UnitE
    = return UnitV

evalExp aid SelfE 
    = return (ActorV aid)

evalExp _ (NumberE x) 
    = return (NumberV x)

--gives an IState Value (\s -> var_val, s')
evalExp _ (VarE name)
    = lookupName name

evalExp selfId (SendE name aps)
    = defineStep $
        do
            msg <- evalExps selfId aps
            ActorV otherId <- lookupName name
            otherInst <- lookupActorInstance otherId
            let newInbox = (_aiInbox otherInst) ++ [msg]
                otherInst' = otherInst { _aiInbox = newInbox }
            isGlobalEnv . geActorInstances %= M.insert otherId otherInst'
            return UnitV

evalExp aid (LetE name exp1 exp2)
    = defineStep $
        do
            v <- evalExp aid exp1
            isLocalEnv . leBindings %= M.insert name v
            evalExp aid exp2

evalExp aid (CreateE name aps)
    = defineStep $
        do 
            behaviour@(Behaviour _ fps _ _) <- lookupBehaviour name
            newId <- isGlobalEnv . geNextAvailableActor <<%= (+1)
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
            isGlobalEnv . geActorInstances %= M.insert newId newActor
            return (ActorV newId)

evalExp aid (PrintE s e)
    = defineStep $
        do
            isLocalEnv . leConsole %= (++ [s])
            evalExp aid e

evalExps :: MonadStepped IState m => ActorId -> [Exp] -> m [Value]
evalExps _ [] 
    = return []

evalExps aid (e : es) 
    = do
        v <- evalExp aid e
        vs <- evalExps aid es
        return (v : vs)
        
lookupName :: MonadStepped IState m => Name -> m Value
lookupName name 
    = do
        bindings <- use (isLocalEnv . leBindings)
        lenv <- use isLocalEnv
        return (M.findWithDefault (error "lookupName: invalid name") name bindings)

lookupActorInstance :: MonadStepped IState m => ActorId -> m ActorInstance
lookupActorInstance aid 
    = do 
        actors <- use (isGlobalEnv . geActorInstances)
        return (M.findWithDefault (error "lookupActorInstance: invalid id") aid actors)

lookupBehaviour :: MonadStepped IState m => Name -> m Behaviour
lookupBehaviour name 
    = do 
        behaviours <- use (isGlobalEnv . geBehaviours)
        return (M.findWithDefault (error "lookupBehaviour: unbound behaviour") name behaviours)
