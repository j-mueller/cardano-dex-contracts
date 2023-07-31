{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module ErgoDex.Plutus where

import           PlutusLedgerApi.V1.Value (AssetClass)
import qualified PlutusLedgerApi.V1.Value as Value
import           PlutusLedgerApi.V2       (ScriptContext, TxInInfo, Value,
                                           scriptContextTxInfo,
                                           txInInfoResolved, txInfoInputs,
                                           txOutValue)

adaAssetClass :: AssetClass
adaAssetClass = Value.assetClass Value.adaSymbol Value.adaToken

valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

inputsNum :: ScriptContext -> Int
inputsNum sCtx = length $ txInfoInputs $ scriptContextTxInfo sCtx
