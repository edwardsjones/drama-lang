{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where 

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Default.Class
import Data.String
import Data.Text.Lazy (Text)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict            as M
import qualified Happy                      as P
import qualified Interpreter                as I
import qualified ServerTypes                as S
import qualified System.Random              as R
import qualified Tokens                     as L
import qualified Types                      as T

import Network.Wai.Middleware.Cors
import Web.Scotty.Trans

newtype AppState 
    = AppState { serverTickets :: M.Map Int [(Maybe (I.Stepped I.IState ()))] }

instance Default AppState where
    def 
        = AppState M.empty

newtype WebM a 
    = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
    sync <- newTVarIO def
    let runActionToIO m = runReaderT (runWebM m) sync
    scottyT 5000 runActionToIO app

app :: ScottyT Text WebM ()
app = do
    middleware simpleCors
    post "/programs" $ do
        progByteStr <- body
        num <- liftIO $ (R.randomRIO (0,30000) :: IO Int)
        let progStr = B.unpack progByteStr
            ast = P.parseDrama $ L.alexScanTokens progStr
            (maybeSusp, is) = setupProgram ast
            clientTicket = S.ClientTicket { S.ticketID = num, S.state = is, S.ready = [0] }
        webM $ modify $ \as -> let newMap = M.insert num [maybeSusp] (serverTickets as)
                               in as { serverTickets = newMap }
        json clientTicket

    post "/step" $ do
        ticket <- jsonData :: ActionT Text WebM S.ClientTicket
        serverTix <- webM $ gets serverTickets
        let suspID = S.ticketID ticket
            is = S.state ticket
            maybeSusp = ticketLookup suspID serverTix
        case maybeSusp of
            Just susp   ->  let (nextSusp, is') = I.step susp is
                                readyList = getReadyActors is'
                                newTicket = S.ClientTicket { S.ticketID = suspID, S.state = is', S.ready = readyList }
                            in do
                                --change list to have most recent susp first
                                webM $ modify $ \as -> as { serverTickets = M.insertWith (++) suspID [nextSusp] (serverTickets as) }
                                json newTicket
            Nothing     ->  do 
                                liftIO $ print (M.size serverTix) 
                                text "done"

    -- Roll back the susps one step
    post "/back" $ do
        ticket <- jsonData :: ActionT Text WebM S.ClientTicket
        let suspID = S.ticketID ticket
            is = S.state ticket
            readyList = getReadyActors is
            newTicket = S.ClientTicket { S.ticketID = suspID, S.state = is, S.ready = readyList }
        webM $ modify $ \as -> as { serverTickets = M.adjust tail suspID (serverTickets as) }
        json newTicket 

-- Takes a Program, and returns a Maybe (Stepped s a) and the state after 
-- the first actor has been instantiated
setupProgram :: T.Program -> (Maybe (I.Stepped I.IState ()), I.IState)
setupProgram (T.Program bs inst) 
    = I.step firstStepped is
    where 
        genv = I.initialEnv bs
        is = I.IState { I._isGlobalEnv = genv, I._isCurrentAID = 0 }
        firstStepped = (I.instantiate inst >> I.scheduler)

ticketLookup :: Int -> M.Map Int [(Maybe (I.Stepped I.IState ()))] -> Maybe (I.Stepped I.IState ())
ticketLookup id tickets
    = case lookupResult of
        Just susps  -> head susps
        Nothing     -> Nothing
    where 
        -- produces a Maybe [Maybe _]
        lookupResult = M.lookup id tickets

getReadyActors :: I.IState -> [Int]
getReadyActors is
    = M.keys readyActors
    where
        genv = I._isGlobalEnv is
        allActors = I._geActorInstances genv
        readyActors = M.filter I.isReady allActors
