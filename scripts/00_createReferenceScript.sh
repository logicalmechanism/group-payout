#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# set up the temp folder
mkdir -p tmp
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

# script paths
script_path="../group-payout-contract/group-payout-contract.plutus"

# Addresses
reference_address=$(cat wallets/reference-wallet/payment.addr)

# lock contract min utxo
min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-reference-script-file ${script_path} \
    --tx-out="${reference_address} + 5000000" | tr -dc '0-9')
echo "Locking Contract Min Fee" ${min_utxo}

script_reference_utxo="${reference_address} + ${min_utxo}"

echo -e "\nCreating Payout Reference:\n" ${script_reference_utxo}
#
# exit
#
# reference wallet pays for the references
echo -e "\033[0;36m\nGathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${reference_address} \
    --out-file tmp/reference_utxo.json

TXNS=$(jq length tmp/reference_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${reference_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/reference_utxo.json)
ref_tx_in=${TXIN::-8}

# chain the set of reference scripts
echo -e "\033[0;36m Building Tx \033[0m"
starting_seller_lovelace=$(jq '[.. | objects | .lovelace] | add' tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${ref_tx_in} \
    --tx-out="${reference_address} + ${starting_seller_lovelace}" \
    --tx-out="${script_reference_utxo}" \
    --tx-out-reference-script-file ${script_path} \
    --fee 900000

FEE=$(cardano-cli transaction calculate-min-fee \
    --tx-body-file tmp/tx.draft \
    --testnet-magic ${testnet_magic} \
    --protocol-params-file tmp/protocol.json \
    --tx-in-count 0 \
    --tx-out-count 0 \
    --witness-count 1)
fee=$(echo $FEE | rev | cut -c 9- | rev)
echo FEE: $FEE

firstReturn=$((${starting_seller_lovelace} - ${min_utxo} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --tx-in ${ref_tx_in} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${script_reference_utxo}" \
    --tx-out-reference-script-file ${script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/reference-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx-1.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx-1.signed

# create a tx reference to be used later
cp tmp/tx-1.signed tmp/tx-reference-utxo.signed