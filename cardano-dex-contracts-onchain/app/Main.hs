module Main where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Short  as BSS
import           Data.Foldable          (forM_)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified ErgoDex.PValidators    as Validators
import           Plutarch.Script        (Script (..))
import           PlutusLedgerApi.Common (serialiseUPLC)
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)
import           System.FilePath        ((</>))

main :: IO ()
main = getArgs >>= \case
    [folder] -> do
        putStrLn $ "Writing scripts to " <> folder
        forM_ scripts $ \(scriptName, script) -> do
            let fn1 = folder </> scriptName <> ".uplc"
            case script of
                Left text -> putStrLn (scriptName <> " failed with " <> Text.unpack text)
                Right (Script script_) -> BS.writeFile fn1 (BSS.fromShort $ serialiseUPLC script_)

    _ -> putStrLn "usage: cardano-dex-contracts-cli FOLDER" >> exitFailure

scripts :: [(String, Either Text Script)]
scripts =
    [ ("pool", Validators.poolValidator)
    , ("deposit", Validators.depositValidator)
    , ("swap", Validators.swapValidator)
    , ("redeem", Validators.redeemValidator)
    ]
