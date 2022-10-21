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

module Raffle.OnChain
    ( raffleValidator
    , raffleScript
    , raffleSBS
    , raffleSerialised
    , Raffle
) where


import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Ledger
import qualified Ledger.Ada             as Ada hiding (divide)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value         as Value
import           Ledger.TimeSlot
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                as Haskell
import           Text.Printf          (printf)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Raffle.Types
import           Codec.Serialise

validateRaffle :: PubKeyHash -> RaffleData -> RaffleAction -> ScriptContext -> Bool
validateRaffle master datum redeemer ctx = case redeemer of
        OffChainSign ->
            traceIfFalse "must be signed by Labs" (isSignedBy master)
        where
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx
            
            isSignedBy :: PubKeyHash -> Bool
            isSignedBy pkh = txSignedBy txInfo pkh

data Raffle
instance Scripts.ValidatorTypes Raffle where
    type instance RedeemerType Raffle = RaffleAction
    type instance DatumType Raffle = RaffleData

raffleValidator :: PubKeyHash -> Scripts.TypedValidator Raffle
raffleValidator params = Scripts.mkTypedValidator @Raffle
    ($$(PlutusTx.compile [|| validateRaffle ||])
        `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @RaffleData @RaffleAction

raffleValidatorScript :: PubKeyHash -> Validator
raffleValidatorScript = Scripts.validatorScript . raffleValidator

raffleScript :: PubKeyHash -> Plutus.Script
raffleScript = Plutus.unValidatorScript . raffleValidatorScript

raffleSBS :: PubKeyHash -> SBS.ShortByteString
raffleSBS =  SBS.toShort . LBS.toStrict . serialise . raffleScript

raffleSerialised :: PubKeyHash -> PlutusScript PlutusScriptV1
raffleSerialised = PlutusScriptSerialised . raffleSBS
