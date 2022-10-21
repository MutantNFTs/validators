{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}

module Raffle.Types
  where

import Data.Data
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell

-- Datums.
data RaffleData =
    RaffleData
        { owner                :: !PubKeyHash       -- The Raffle owner
        , entryPolicyId        :: !CurrencySymbol   -- Entry Currency Symbol
        , entryTokenName       :: !TokenName        -- Entry Token name
        , entryCost            :: !Integer          -- Entry cost
        }
    deriving (Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''RaffleData [ ('RaffleData, 0)]
PlutusTx.makeLift ''RaffleData

-- Redeemers.
data RaffleAction = OffChainSign
    deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''RaffleAction [ ('OffChainSign,   0)]
PlutusTx.makeLift ''RaffleAction
