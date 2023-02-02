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

The contract assumes the entry transaction validates the datum for equal length arrays or the UTxO will be perma locked. A successful validation will pay N addresses with some amount of lovelace greater than the minimum lovelace. The goal of the contract is to maximize the number of payout addresses a contract can perform as a proof of concept. This contract improves the the maximum payout address over the non-optimized payout contract by a factor of 3.

## Set Up

The test scripts assume that a payout.json file is filled with proper information like the example below.

```json
{
    "addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp":2000000,
    "addr_test1qrure9p250hme6tjwj69x63azuvq9ng5k6mwc6m9002wsfvcxrxxytt2067pfqzygxf7446yxhtrjx9f9vqhz0az003qgne52c":2000000,
    "addr_test1vryusxht8rgz4g6twrjz4y8gss66w202vtfyk84wahmguzgh5mejc":1234567
}
```

In this example, three addresses are being paid various amount of lovelace. The createPayout.sh script will auto calculate the total lovelace for the UTxO and will create the proper datum for that transaction. The completePayout.sh script will use the payout.json file to build the correct outputs for the validation logic to pass. Both base and enterprise addresses work with this contract. It is important that the minimum ADA is satisfied inside the payouts and that any duplicate addresses get combined into a single line. These type of validations should occur in the off chain for preparing an entry payout UTxO. If these precautions are not followed then the UTxO may be stuck due to the inability to pay someone below minimum ADA.

### Stress Test

Payout of 28 addresses from the contract.

Mem 13,176,772
Steps 3,445,668,255

A massive improvement!

```
3803304ecd5e26f5027f847ef43c8012777f99273799e2d365dfd9e58b236c7d
```

[Cardanoscan](https://preprod.cardanoscan.io/transaction/3803304ecd5e26f5027f847ef43c8012777f99273799e2d365dfd9e58b236c7d)