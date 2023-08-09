module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    wrapValidator,
) where

import qualified ErgoDex.PContracts.PDeposit as PD
import qualified ErgoDex.PContracts.PPool    as PP
import qualified ErgoDex.PContracts.PRedeem  as PR
import qualified ErgoDex.PContracts.PSwap    as PS

import Plutarch
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Internal
import Data.Text (Text)

cfgForValidator :: Config
cfgForValidator = Config NoTracing

wrapValidator ::
    (PIsData dt, PIsData rdmr) =>
    Term s (dt :--> rdmr :--> PScriptContext :--> PBool) ->
    Term s (PData :--> PData :--> PScriptContext :--> POpaque)
wrapValidator validator = plam $ \datum redeemer ctx ->
    let dt = pfromData $ punsafeCoerce datum
        rdmr = pfromData $ punsafeCoerce redeemer
        result = validator # dt # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Validator reduced to False")

poolValidator :: Either Text Script
poolValidator = compile cfgForValidator $ wrapValidator PP.poolValidatorT

swapValidator :: Either Text Script
swapValidator = compile cfgForValidator $ wrapValidator PS.swapValidatorT

depositValidator :: Either Text Script
depositValidator = compile cfgForValidator $ wrapValidator PD.depositValidatorT

redeemValidator :: Either Text Script
redeemValidator = compile cfgForValidator $ wrapValidator PR.redeemValidatorT
