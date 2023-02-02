rm validator.addr
rm validator.bytes
rm validator.hash
rm group-payout-contract.plutus
#
cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run group-payout-contract
#
cardano-cli address build --payment-script-file group-payout-contract.plutus --testnet-magic 1 --out-file validator.addr
cardano-cli transaction policyid --script-file group-payout-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
#
echo -e "\nValidator Testnet Address:" $(cat validator.addr)
echo -e "\nValidator Hash:" $(cat validator.hash)
echo -e "\nValidator Bytes:" $(cat validator.bytes)
echo "DONE"