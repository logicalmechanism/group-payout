#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

# script
script_path="../group-payout-contract/group-payout-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

# the staker
payer_address=$(cat wallets/seller-wallet/payment.addr)
payer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)

# update payout-datum.json with data from payout.json
python createDatum.py

# get total ada value
total_ada=$(jq -r 'map(.) | add' payout.json)
script_address_out="${script_address} + ${total_ada}"
echo "Stake OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Staker UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${payer_address} \
    --out-file tmp/payer_utxo.json

TXNS=$(jq length tmp/payer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${payer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/payer_utxo.json)
payer_tx_in=${TXIN::-8}

# get script reference tx
script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${payer_address} \
    --tx-in ${payer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datum/payout-datum.json \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed
