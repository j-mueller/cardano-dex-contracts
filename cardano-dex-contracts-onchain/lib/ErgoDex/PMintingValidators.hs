{-# LANGUAGE OverloadedStrings #-}

module ErgoDex.PMintingValidators (
    poolNftMiningValidator,
    poolLqMiningValidator,
    poolStakeChangeMintPolicyValidator,
    wrapMintingValidator,
) where

import Plutarch
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Data.Text (Text)
import qualified ErgoDex.PContracts.PAssets as A
import ErgoDex.PContracts.PPoolStakeChangeMintPolicy
import PlutusLedgerApi.V1.Value   (TokenName(..), AssetClass(..))
import PlutusLedgerApi.V1.Crypto  (PubKeyHash)
import PlutusLedgerApi.V1.Contexts

cfgForMintingValidator :: Config
cfgForMintingValidator = Config NoTracing

wrapMintingValidator ::
    PIsData rdmr =>
    ClosedTerm (rdmr :--> PScriptContext :--> PBool) ->
    ClosedTerm (PData :--> PScriptContext :--> POpaque)
wrapMintingValidator validator = plam $ \rdmr' ctx ->
    let rdmr = pfromData $ punsafeCoerce rdmr'
        result = validator # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Minting validator reduced to False")

poolNftMiningValidator :: TxOutRef -> TokenName -> Either Text Script
poolNftMiningValidator oref tn =
    compile cfgForMintingValidator $
        wrapMintingValidator $
            A.poolNftMintValidatorT (pconstant oref) (pconstant tn)

poolLqMiningValidator :: TxOutRef -> TokenName -> Integer -> Either Text Script
poolLqMiningValidator oref tn emission =
    compile cfgForMintingValidator $
        wrapMintingValidator $
            A.poolLqMintValidatorT (pconstant oref) (pconstant tn) (pconstant emission)

poolStakeChangeMintPolicyValidator :: AssetClass -> [PubKeyHash] -> Integer -> Either Text Script
poolStakeChangeMintPolicyValidator ac stakeAdminPkh threshold = 
    compile cfgForMintingValidator $ 
        wrapMintingValidator $
            poolStakeChangeMintPolicyValidatorT (pconstant ac) (pconstant stakeAdminPkh) (pconstant threshold)