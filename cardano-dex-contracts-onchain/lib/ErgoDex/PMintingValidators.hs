module ErgoDex.PMintingValidators (
    poolNftMiningValidator,
    poolLqMiningValidator,
    wrapMintingValidator,
) where

import           Plutarch
import           Plutarch.Api.V2.Contexts    (PScriptContext)
import           Plutarch.Prelude
import           Plutarch.Unsafe             (punsafeCoerce)

import           Data.Text                   (Text)
import qualified ErgoDex.PContracts.PAssets  as A
import           PlutusLedgerApi.V1.Contexts
import           PlutusLedgerApi.V1.Value    (TokenName)

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