import Prelude
import Cardano.Api
import GroupPayoutContract ( groupPayoutContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "group-payout-contract.plutus" Nothing groupPayoutContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
