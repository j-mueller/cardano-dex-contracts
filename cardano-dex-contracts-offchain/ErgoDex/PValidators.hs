module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    simpleStakingValidator,
    lockPkhStakingValidator
) where

import qualified Cardano.Api                          as C
import           Cardano.Binary                       (DecoderError)
import           Control.Monad.IO.Class               (MonadIO (..))
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Proxy                           (Proxy (..))
import           Paths_cardano_dex_contracts_offchain

poolValidatorDataFileName :: String
poolValidatorDataFileName = "pool.uplc"

poolValidator :: (MonadIO m) => m (Either DecoderError (C.Script C.PlutusScriptV2))
poolValidator = readValidatorFromFile poolValidatorDataFileName

swapValidatorDataFileName :: String
swapValidatorDataFileName = "swap.uplc"

swapValidator :: (MonadIO m) => m (Either DecoderError (C.Script C.PlutusScriptV2))
swapValidator = readValidatorFromFile swapValidatorDataFileName

depositValidatorDataFileName :: String
depositValidatorDataFileName = "deposit.uplc"

depositValidator :: (MonadIO m) => m (Either DecoderError (C.Script C.PlutusScriptV2))
depositValidator = readValidatorFromFile depositValidatorDataFileName

redeemValidatorDataFileName :: String
redeemValidatorDataFileName = "redeem.uplc"

redeemValidator :: (MonadIO m) => m (Either DecoderError (C.Script C.PlutusScriptV2))
redeemValidator = readValidatorFromFile redeemValidatorDataFileName

simpleStakingValidatorDataFileName :: String
simpleStakingValidatorDataFileName = "simpleStaking.uplc"

simpleStakingValidator :: (MonadIO m) => m (Either DecoderError (C.Script C.PlutusScriptV2))
simpleStakingValidator = readValidatorFromFile simpleStakingValidatorDataFileName

lockPkhStakingValidatorDataFileName :: String
lockPkhStakingValidatorDataFileName = "stakinWithPkh.uplc"

lockPkhStakingValidator :: (MonadIO m) => m (Either DecoderError (C.Script C.PlutusScriptV2))
lockPkhStakingValidator = readValidatorFromFile lockPkhStakingValidatorDataFileName

readValidatorFromFile :: (MonadIO m) => String -> m (Either DecoderError (C.Script C.PlutusScriptV2))
readValidatorFromFile dataFieldName = do
    path  <- liftIO $ getDataFileName dataFieldName
    bytes <- liftIO $ BSL.readFile path
    pure $ C.deserialiseFromCBOR (C.proxyToAsType Proxy) (BSL.toStrict bytes)
