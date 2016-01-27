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
        }

    deriving (Generic, ToJSON, FromJSON, Eq, Show)
