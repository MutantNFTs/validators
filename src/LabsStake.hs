{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}

module LabsStake
    ( stakeScript
    , stakeSBS
    , stakeSerialised
) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Text              as T
import qualified Data.Map               as Map
import           GHC.Generics           (Generic)
import           Ledger
import qualified Ledger.Ada             as Ada hiding (divide)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value         as Value
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                as Haskell
import           Text.Printf          (printf)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Codec.Serialise

data StakeData =
    StakeData
        { staker            :: BuiltinByteString }    -- The NFT staker
    deriving (Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''StakeData [('StakeData, 0)]
PlutusTx.makeLift ''StakeData

data StakeAction = Unstake
    deriving (Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''StakeAction [ ('Unstake,       0)]
PlutusTx.makeLift ''StakeAction

{-# INLINABLE validateStake #-}
validateStake :: StakeData -> StakeAction -> ScriptContext -> Bool
validateStake d@StakeData{staker} redeemer ctx = case redeemer of
        Unstake ->
            traceIfFalse "must be signed by the staker" $ txSignedBy txInfo stakerPubKeyHash
        where
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx

            stakerPubKeyHash :: PubKeyHash
            stakerPubKeyHash = PubKeyHash staker

data Stake
instance Scripts.ValidatorTypes Stake where
    type instance RedeemerType Stake = StakeAction
    type instance DatumType Stake = StakeData

stakeValidator :: Scripts.TypedValidator Stake
stakeValidator = Scripts.mkTypedValidator @Stake
    $$(PlutusTx.compile [|| validateStake ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @StakeData @StakeAction

stakeValidatorScript :: Validator
stakeValidatorScript = Scripts.validatorScript stakeValidator

stakeScript :: Plutus.Script
stakeScript = Plutus.unValidatorScript stakeValidatorScript

stakeSBS :: SBS.ShortByteString
stakeSBS =  SBS.toShort . LBS.toStrict $ serialise stakeScript

stakeSerialised :: PlutusScript PlutusScriptV1
stakeSerialised = PlutusScriptSerialised stakeSBS
