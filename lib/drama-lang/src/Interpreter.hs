{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}

module Interpreter where

import qualified Data.Map.Strict as M
import qualified Data.Aeson      as A
import GHC.Generics
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

    deriving (Generic, A.ToJSON, A.FromJSON, Eq, Show)

type ActorId
    = Int

data GlobalEnv 
    = GlobalEnv 
        { _geNextAvailableActor :: ActorId
        , _geBehaviours :: M.Map Name Behaviour
        , _geActorInstances :: M.Map ActorId ActorInstance
        } 
    
    deriving (Generic, A.ToJSON, A.FromJSON, Eq, Show)

-- By ToJSON and FromJSON can only be derived (for maps) when the key is a 
-- string, hence the jiggerypokery below.
instance A.ToJSON (M.Map ActorId ActorInstance) where
    toJSON map = A.toJSON (M.mapKeys (show) map)

instance A.FromJSON (M.Map ActorId ActorInstance) where
    parseJSON v = M.mapKeys (read :: String -> ActorId) <$> A.parseJSON v

data ActorInstance
    = ActorInstance
        { _aiId :: ActorId
        , _aiInbox :: [Message]
        , _aiBehaviour :: Behaviour
        , _aiEnv :: LocalEnv
        , _aiCanReceive :: Bool
        }

    deriving (Generic, A.ToJSON, A.FromJSON, Eq, Show)

data LocalEnv
    = LocalEnv
        { _leBindings :: M.Map Name Value
        , _leConsole :: [String]
        }

    deriving (Generic, A.ToJSON, A.FromJSON, Eq, Show)

data Value
    = UnitV
    | BoolV Bool
    | NumberV Int
    | StringV String
    | ActorV ActorId
    | ListV [Value]
    | EncryptedV Value String
    deriving (Generic, A.ToJSON, A.FromJSON, Eq, Show)

type Message
    = [Value]

$(makeLenses ''IState)
$(makeLenses ''GlobalEnv)
$(makeLenses ''LocalEnv)
$(makeLenses ''ActorInstance)

-- Takes a value, and produces a function that takes 
-- a state and produces a SteppedResult and the state after
data Stepped s a
    = Stepped (s -> (SteppedResult s a, s))

-- This is how execution is stepped; either the execution is Done, and produces
-- simply the result, or it's Suspended, and has a Stepped function which is 
-- like a continuation; it contains the work left to be done
data SteppedResult s a
    = Done a
    | Suspended (Stepped s a)

-- Takes a Stepped and a state, and applies the Stepped function to the state
-- to produce a SteppedResult
runStepped :: Stepped s a -> s -> (SteppedResult s a, s)
runStepped (Stepped f) s
    = f s

-- To be an instance of the Functor typeclass, you must implement fmap; fmap
-- takes a function (a -> b), and applies it to all the resulting a. 
instance Functor (Stepped s) where
  --fmap :: (a -> b) -> Stepped s a -> Stepped s b
    fmap f (Stepped g)
        = Stepped $

         -- Produces a function which takes a state, and returns the 
         -- SteppedResult with f applied, as well as the new state;
         -- this is equivalent to Stepped s b
            \s ->   let (r, s') = g s

                 -- Uses fmap to apply the function to the SteppedResult
                    in  (fmap f r, s')

-- Need to define fmap for SteppedResult, as used in the definition for fmap
-- in Stepped
instance Functor (SteppedResult s) where

 -- If the result is Done, simply apply the function to the resulting value.
    fmap f (Done a)
        = Done (f a)

 -- If the result is Suspended, fmap f over k (which is of type Stepped), which
 -- will advance the evaluation a step and fmap f over the corresponding result,
 -- untill Done is reached and f can be applied.
    fmap f (Suspended k)
        = Suspended (fmap f k)

instance Applicative (Stepped s) where
 -- Takes a value, and lifts it into the functor. Stepped is also a monad,
 -- so can just steal from the monad definition. 
    pure
        = return

 -- It takes a function inside a functor, and applies it to a values inside a 
 -- functor. The functor in this case is Stepped s.
 -- (<*>) :: Stepped s (a -> b) -> Stepped s a -> Stepped s b
    sf <*> sx
        = do
         -- The binding happening here 'unpacks' the function and values from
         -- there respective functors. Then you just apply the function. 
            f <- sf
            x <- sx
            return (f x)

instance Monad (Stepped s) where
    
 -- Takes a value and put it in the monad. If calling return, then you've 
 -- completed a step, so return Done.
 -- return :: a -> Stepped s a
    return x
        = Stepped $
            \s -> (Done x, s)

 -- Bind takes a value in the monad, a function from a value to a value
 -- in the monad, and produces the function applied to the value in the
 -- monad. 
 -- (>>=) :: Stepped s a -> (a -> Stepped s b) -> Stepped s b
    ma >>= kamb
        = Stepped $

         -- Return a function which takes a state..
            \s -> case runStepped ma s of

             -- If the SteppedResult of applying Stepped to the given state
             -- is Done, then apply the function to the result (kamb x), and
             -- call runStepped with it and the new state. This produces
             -- a Stepped s b.
                (Done x, s') -> runStepped (kamb x) s'

             -- If the SteppedResult is suspended, you don't have a value to 
             -- apply kamb to, so call bind again untill the base case is hit.
             -- This needs to be in a thunk though (i.e. Suspended), in order
             -- to stop execution. 
             -- (Suspended (sa >>= kamb), s') :: (SteppedResult s b, s')
                (Suspended sa, s') -> (Suspended (sa >>= kamb), s')    

-- Stepped is an instance of the State Monad, so I need to implement it's 
-- default methods get and put
instance MonadState s (Stepped s) where

    -- Returns the current state
    get 
        = Stepped $ \s -> (Done s, s)

    -- Changes the state to the one provided
    put newState
        = Stepped $ \_ -> (Done (), newState)

-- In order for a Stateful monad to be a member of the Stepped monads, it must
-- implement defineStep.
class MonadState s m => MonadStepped s m where
    defineStep :: m a -> m a

-- This instance is for when you want to run a program straight through, 
-- without steps; it treats all the defineStep calls as just the id function.
-- The only case we care about is where the inner monad, the one being 
-- transformed, is the identity monad; in this case, MonadStepped just serves
-- as the State monad, allowing us to pass through execution normally. 
instance Monad m => MonadStepped s (StateT s m) where
    defineStep 
        = id

-- Conversely, this defines the defineStep function for when Stepped is being 
-- used.  
instance MonadStepped s (Stepped s) where
 -- defineStep :: Stepped s a -> Stepped s a
    defineStep m
     -- Everytime you see defineStep, make a function (Stepped) which puts
     -- all the rest of the work (m) into a thunk (Suspended).  
        = Stepped $ \s -> (Suspended m, s)

-- Here's where the stepping happens; takes a Stepped (function from a state
-- to a SteppedResult), a state, and produces the continuation and the state
-- after applying Stepped
step :: Stepped s a -> s -> (Maybe (Stepped s a), s)
step (Stepped f) s

    --Apply Stepped to the state, to get a SteppedResult
    = case f s of
        
        -- If it's Done, then no work left; just return the state
        (Done x, s') -> (Nothing, s')

        -- If it's Suspended, return the continuation and the state
        (Suspended k, s') -> (Just k, s')
    
-- Lens which will get the LocalEnv of the actor id specified in the state
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

-- Takes a program, creates the initial state, instantiates the first actor 
-- and calls scheduler. Returns the GlobalEnv after completing execution.
runProgram :: Program -> IO ()
runProgram (Program bs inst)
    = let (_, is') = flip runState is $
            do  instantiate inst
                scheduler
      in prettyPrintState is'
    where
        genv = initialEnv bs
        is = IState { _isGlobalEnv = genv, _isCurrentAID = 0 }

-- Same as runProgram, except it allows the user to step through the execution
-- by way of the stepper function.
stepProgram :: Program -> IO ()
stepProgram (Program bs inst)  
    = let (maybeSusp, is') = step (instantiate inst >> scheduler) is
      in stepper maybeSusp is'
    where
        genv = initialEnv bs
        is = IState { _isGlobalEnv = genv, _isCurrentAID = 0 }

-- Takes a Maybe (Stepped IState a) and the current state, and steps it 
-- forward one step everytime the user presses enter. 
stepper :: (Maybe (Stepped IState a)) -> IState -> IO ()
stepper maybeSusp is
    = case maybeSusp of
        Just susp -> let (nextSusp, is') = step susp is 
                     in do  hSetBuffering stdin NoBuffering
                            input <- getLine
                            prettyPrintState is'
                            stepper nextSusp is'
        Nothing   -> putStrLn "Done"

prettyPrintState :: IState -> IO ()
prettyPrintState is
    = let currentActorString = "Current Actor: " ++ (show (_isCurrentAID is)) 
      in do 
        putStrLn "========================================"
        putStrLn currentActorString
        prettyPrintGlobalEnv (_isGlobalEnv is)


-- Pretty printer for the command line interface
prettyPrintGlobalEnv :: GlobalEnv -> IO ()
prettyPrintGlobalEnv genv
    = let nextActorString = "Next Available Actor ID: " ++ (show nextAvailableActor)
          actorStringList = M.foldl (\a -> \str -> a ++ str) "" (M.map formatInstance actorList)
      in do putStrLn nextActorString
            putStrLn actorStringList
    where
        nextAvailableActor = _geNextAvailableActor genv
        actorList = _geActorInstances genv
        
-- Helper function which takes an actor instance and produces a string 
-- with all of it's details
formatInstance :: ActorInstance -> String
formatInstance (ActorInstance id inbox (Behaviour name _ _ _) (LocalEnv bindings console) rec)
    = "\nActor ID: " ++ (show id) ++ "\nInbox: " ++ (show inbox) ++ "\nBehaviour: " ++  (show name) ++ "\nBindings: " ++ (show (M.toList bindings)) ++ "\nConsole: " ++ (show console) ++ "\nReady to receive: " ++ (show rec) ++ "\n"

-- Handles which expression should be evaluated next; this expression will be
-- either a Receive or PreReceive expression. After evaluation, it calls itself
-- Currently, the actor that is chosen to be evaluated next is just the first 
-- in the list returned by the getReady function. Will stop when there are no
-- actors ready.
scheduler :: MonadStepped IState m => m ()
scheduler
    = do 
        is <- get
        let genv = _isGlobalEnv is
            allIds = M.keys (_geActorInstances genv)
        ready <- getReady allIds
        if null ready 
            
            -- If no ready actors, then finished.
            then return ()

            -- Else, evaluate some expression. Step is defined here, so that
            -- the user has the chance to change which actor executes next.
            else defineStep $
                do 
                    is' <- get
                    let actorID = checkID is' ready
                    actor <- lookupActorInstance actorID
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

-- Evaluates the given PreReceive expression in the given ActorId's LocalEnv
preReceive :: MonadStepped IState m => ActorId -> PreReceive -> m Value
preReceive aid pr 
    = do
        v <- evalExp aid pr
        actor <- lookupActorInstance aid
        let newActor = actor { _aiCanReceive = True }
        isGlobalEnv . geActorInstances %= M.update (replaceActor newActor) (_aiId actor) 
        return v

-- Used for updating actors in the GlobalEnv
replaceActor :: ActorInstance -> ActorInstance -> Maybe ActorInstance
replaceActor newActor oldActor = if oldActor == oldActor then Just newActor else Nothing

-- Takes an ActorInstance and it's message handling expressions, and 
-- evaluates the next available message in that actor's inbox
receive :: MonadStepped IState m => ActorInstance -> [Receive] -> m Value
receive ai rec 
    = do
        -- Remove message from inbox, and update the actor instance
        let (aps : msgs) = _aiInbox ai
            --ai' is the actor without the msg about to be handled
            ai' = ai { _aiInbox = msgs }
            aid = _aiId ai
        isGlobalEnv . geActorInstances %= M.update (replaceActor ai') (_aiId ai)

        -- Find handler exp to use, and bind msg params
        let (fps, handling) = matchArity rec aps
            bindings = M.fromList (zip fps aps)

        -- Store env withouth msg bindings for after
        lenv <- use isLocalEnv

        isLocalEnv . leBindings %= M.union bindings
        v <- evalExp aid handling

        ai'' <- lookupActorInstance aid

        -- Remove msg bindings
        isLocalEnv . leBindings .= _leBindings lenv
        lenv2 <- use isLocalEnv
        let ai''' = ai'' { _aiEnv = lenv2 }
        isGlobalEnv . geActorInstances %= M.update (replaceActor ai''') (_aiId ai''')

        return v

-- Used for handling messages; messages are currently pattern matched on arity
-- Takes the receive statements of an actor, and the message to be handled, 
-- and returns the receive statement to be used
matchArity :: [Receive] -> Message -> Receive
matchArity [] _ = error "matchArity: no matching cases for message"
matchArity (r@(fps, exp) : rs) msg = if length fps == length msg then r else matchArity rs msg

-- Creates the initial GlobalEnv if provided with the list of behaviours
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

-- Returns a list of behaviour names
getNames :: [Behaviour] -> [Name]
getNames [] = []
getNames ((Behaviour name _ _ _) : bs) = name : getNames bs

-- Creates the first actor, specified in the last line of a given drama program
-- A dummy actorId 0 is provided to evalExp, so that the first actor can be 
-- created; as there is no LocalEnv needed for the first actor, it is not used
instantiate :: MonadStepped IState m => Instantiation -> m ActorId
instantiate (Instantiation name aps)
    = do
        ActorV firstId <- evalExp 0 (CreateE name aps) 
        return firstId

-- Gets the list of actors ready to evaluate expressions, whether they be
-- messages or PreReceive expressions
getReady :: MonadStepped IState m => [ActorId] -> m [ActorId]
getReady [] = return []
getReady (aid : aids)
    = do
        actorInst <- lookupActorInstance aid
        readyAids <- getReady aids
        return (if isReady actorInst then aid : readyAids else readyAids)

isReady :: ActorInstance -> Bool
isReady ai = not (_aiCanReceive ai && null (_aiInbox ai))

-- Used to check if the isCurrentAID is ready or not; this is the value
-- changed by the server to indicate which actor to execute next. This
-- function makes it so that the CLI still works (janky, but hey). 
checkID :: IState -> [ActorId] -> ActorId
checkID is ready
    = if (elem currentAID ready) 
        then currentAID
        else head ready    
    where 
        currentAID = _isCurrentAID is

-- Evaluates a given expression in the environment of the actor whose ID is 
-- provided as argument, and returns the result
evalExp :: MonadStepped IState m => ActorId -> Exp -> m Value
evalExp _ UnitE
    = return UnitV

evalExp aid SelfE 
    = return (ActorV aid)

evalExp _ (BoolE b)
    = if b == "true" then return (BoolV True) else return (BoolV False)

evalExp _ (NumberE x) 
    = return (NumberV x)

evalExp _ (StringE s)
    = return (StringV s)

evalExp _ (VarE name)
    = lookupName name

evalExp aid (ListE exps)
    = do
        vals <- evalExps aid exps
        return (ListV vals)

evalExp aid (ListOperationE op xs)
    = do
        vs@(ListV l) <- evalExp aid xs
        case op of
            "tail"  ->  return (ListV (tail l))
            "init"  ->  return (ListV (init l))
            "head"  ->  return (head l)
            "last"  ->  return (last l)
            "length"->  return (NumberV (length l))

evalExp aid (ConsE exp var)
    = do
        xs@(ListV v) <- lookupName var
        val <- evalExp aid exp
        return (ListV (val : v))
        

evalExp selfId (SendE name aps)
    = do
        msg <- evalExps selfId aps
        ActorV otherId <- lookupName name
        otherInst <- lookupActorInstance otherId
        let newInbox = (_aiInbox otherInst) ++ [msg]
            otherInst' = otherInst { _aiInbox = newInbox }
        isGlobalEnv . geActorInstances %= M.insert otherId otherInst'
        return UnitV

evalExp aid (LetE name exp1 exp2)
    = do
        v <- evalExp aid exp1
        isLocalEnv . leBindings %= M.insert name v
        evalExp aid exp2

evalExp aid (CreateE name aps)
    = do 
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
    = do
        isLocalEnv . leConsole %= (++ [s])
        evalExp aid e

evalExp aid (IfE condition exp1 exp2)
    = do
        -- Get boolean of condition
        conditionResult@(BoolV b) <- evalExp aid condition
        -- exp1 if True, exp2 if False
        if b then evalExp aid exp1 else evalExp aid exp2

evalExp aid (EqualityE exp1 exp2 operator) 
    = do
        v1 <- evalExp aid exp1
        v2 <- evalExp aid exp2
        case (v1, v2) of
            (NumberV n1, NumberV n2)    
                -> case operator of
                    "=="    -> return (BoolV (n1 == n2))
                    "!="    -> return (BoolV (not (n1 == n2)))
                    ">"     -> return (BoolV (n1 > n2))
                    "<"     -> return (BoolV (n1 < n2))
                    ">="    -> return (BoolV (n1 >= n2))
                    "<="    -> return (BoolV (n1 <= n2))
            _
                -> case operator of
                    "=="    -> return (BoolV (v1 == v2))
                    "!="    -> return (BoolV (not (v1 == v2)))

evalExp aid (ArithmeticE exp1 exp2 operator)
    = do
        v1@(NumberV n1) <- evalExp aid exp1
        v2@(NumberV n2) <- evalExp aid exp2
        case operator of
            "-"     -> return (NumberV (n1 - n2))
            "+"     -> return (NumberV (n1 + n2))
            "/"     -> return (NumberV (div n1 n2)) 
            "*"     -> return (NumberV (n1 * n2))

evalExp aid (EncryptE var key)
    = do
        value <- lookupName var
        return (EncryptedV value key)

evalExp _ (DecryptE enc givenKey)
    = do
        encValue@(EncryptedV value actualKey) <- lookupName enc
        if givenKey == actualKey then return value else error "Wrong key used to decrypt."

evalExps :: MonadStepped IState m => ActorId -> [Exp] -> m [Value]
evalExps _ [] 
    = return []

evalExps aid (e : es) 
    = do
        v <- evalExp aid e
        vs <- evalExps aid es
        return (v : vs)
        
-- Takes a name and looks up the binding for that name in the current LocalEnv
lookupName :: MonadStepped IState m => Name -> m Value
lookupName name 
    = do
        bindings <- use (isLocalEnv . leBindings)
        lenv <- use isLocalEnv
        return (M.findWithDefault (error "lookupName: invalid name") name bindings)

-- Takes an ActorId and returns the ActorInstance with that ID
lookupActorInstance :: MonadStepped IState m => ActorId -> m ActorInstance
lookupActorInstance aid 
    = do 
        actors <- use (isGlobalEnv . geActorInstances)
        return (M.findWithDefault (error "lookupActorInstance: invalid id") aid actors)

-- Takes a name and looks up the corresponding behaviour
lookupBehaviour :: MonadStepped IState m => Name -> m Behaviour
lookupBehaviour name 
    = do 
        behaviours <- use (isGlobalEnv . geBehaviours)
        return (M.findWithDefault (error "lookupBehaviour: unbound behaviour") name behaviours)
