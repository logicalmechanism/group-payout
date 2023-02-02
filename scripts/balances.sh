#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# script
script_path="../group-payout-contract/group-payout-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

# wallet info
staker_address=$(cat wallets/seller-wallet/payment.addr)
reference_address=$(cat wallets/reference-wallet/payment.addr)
collat_address=$(cat wallets/collat-wallet/payment.addr)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

# script
echo -e "\033[1;35m Script Address:" 
echo -e "\n${script_address}\n";
${cli} query utxo --address ${script_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

# staker / reward
echo -e "\033[1;36m Staker Address:" 
echo -e "\n${staker_address}\n";
${cli} query utxo --address ${staker_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

# reference
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${reference_address}\n";
${cli} query utxo --address ${reference_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"

# collat
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${collat_address}\n";
${cli} query utxo --address ${collat_address} --testnet-magic ${testnet_magic}
echo -e "\033[0m"
