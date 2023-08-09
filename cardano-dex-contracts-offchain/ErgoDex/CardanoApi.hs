{-# LANGUAGE PackageImports #-}
-- | Bridge code to allow using the contracts with @cardano-api@ types
module ErgoDex.CardanoApi(

    -- * Spending validators
    poolScript,
    depositScript,
    swapScript,
    redeemScript,

    -- * Minting validators
    poolNftMintingScript,
    poolLqMintingScript,

    -- * Conversion types
    CardanoApiScriptError(..),
    plutarchScriptToCapiScript
) where

import qualified Cardano.Api                                                as C
import           Cardano.Binary                                             (DecoderError)
import           Control.Monad                                              ((>=>))
import           Data.Bifunctor                                             (Bifunctor (..))
import qualified Data.ByteString.Short                                      as BSS
import           Data.Proxy                                                 (Proxy (..))
import           Data.Text                                                  (Text)
import qualified "cardano-dex-contracts-onchain" ErgoDex.PMintingValidators as Validators
import qualified "cardano-dex-contracts-onchain" ErgoDex.PValidators        as Validators
import           Plutarch.Script                                            (Script (..))
import           PlutusLedgerApi.Common                                     (serialiseUPLC)
import           PlutusLedgerApi.V1.Contexts                                (TxOutRef)
import           PlutusLedgerApi.V1.Value                                   (TokenName)

data CardanoApiScriptError =
    PlutarchScriptError Text
    | CapiDeserialiseError DecoderError
    deriving Show

poolScript :: Either CardanoApiScriptError (C.Script C.PlutusScriptV2)
poolScript = mkScript Validators.poolValidator

depositScript :: Either CardanoApiScriptError (C.Script C.PlutusScriptV2)
depositScript = mkScript Validators.depositValidator

swapScript :: Either CardanoApiScriptError (C.Script C.PlutusScriptV2)
swapScript = mkScript Validators.swapValidator

redeemScript :: Either CardanoApiScriptError (C.Script C.PlutusScriptV2)
redeemScript = mkScript Validators.redeemValidator

poolNftMintingScript :: TxOutRef -> TokenName -> Either CardanoApiScriptError (C.Script C.PlutusScriptV2)
poolNftMintingScript outRef = mkScript . Validators.poolNftMiningValidator outRef

poolLqMintingScript :: TxOutRef -> TokenName -> Integer -> Either CardanoApiScriptError (C.Script C.PlutusScriptV2)
poolLqMintingScript outRef i = mkScript . Validators.poolLqMiningValidator outRef i

plutarchScriptToCapiScript :: Script -> Either DecoderError (C.Script C.PlutusScriptV2)
plutarchScriptToCapiScript (Script k) = C.deserialiseFromCBOR (C.proxyToAsType Proxy) $ BSS.fromShort $ serialiseUPLC k

mkScript :: Either Text Script -> Either CardanoApiScriptError (C.Script C.PlutusScriptV2)
mkScript = first PlutarchScriptError >=> first CapiDeserialiseError . plutarchScriptToCapiScript
