# Group Payout

The contract expects this datum structure.

```hs
data CustomDatumType = CustomDatumType
  { pkhs :: [V2.PubKeyHash]
  -- ^ The public key hahses
  , scs  :: [V2.PubKeyHash]
  -- ^ The staking credentials
  , amts :: [Integer]
  -- ^ The lovelace amounts
  }
```

The contract assumes the entry transaction validates the datum for equal length arrays or the UTxO will be perma locked. The contract will pay N addresses with some amount of lovelace greater than the minimum lovelace. The goal of the contract is to maximize the number of payout addresses as a proof of concept.

This contract improves the the maximum payout address over the non-optimized contract by a factor of 3.

