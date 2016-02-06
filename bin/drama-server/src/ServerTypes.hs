{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ServerTypes where

import Data.Aeson
import GHC.Generics
import Interpreter

data ClientTicket
    = ClientTicket
        { ticketID :: Int
        , state :: IState
        , ready :: [Int]
        }

    deriving (Generic, ToJSON, FromJSON, Eq, Show)
