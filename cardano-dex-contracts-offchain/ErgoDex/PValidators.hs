module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
) where

import PlutusLedgerApi.Common (SerialisedScript)
import           Codec.Serialise
import qualified Data.ByteString.Lazy   as BSL
import           Control.Monad.IO.Class
import           Paths_cardano_dex_contracts_offchain

poolValidatorDataFileName :: String
poolValidatorDataFileName = "pool.uplc"

poolValidator :: (MonadIO m) => m SerialisedScript
poolValidator = readValidatorFromFile poolValidatorDataFileName

swapValidatorDataFileName :: String
swapValidatorDataFileName = "swap.uplc"

swapValidator :: (MonadIO m) => m SerialisedScript
swapValidator = readValidatorFromFile swapValidatorDataFileName

depositValidatorDataFileName :: String
depositValidatorDataFileName = "deposit.uplc"

depositValidator :: (MonadIO m) => m SerialisedScript
depositValidator = readValidatorFromFile depositValidatorDataFileName

redeemValidatorDataFileName :: String
redeemValidatorDataFileName = "redeem.uplc"

redeemValidator :: (MonadIO m) => m SerialisedScript
redeemValidator = readValidatorFromFile redeemValidatorDataFileName

readValidatorFromFile :: (MonadIO m) => String -> m SerialisedScript
readValidatorFromFile dataFieldName = do
    path  <- liftIO $ getDataFileName dataFieldName
    bytes <- liftIO $ BSL.readFile path
    pure $ deserialise bytes