import json
import subprocess

def json_convert(pkhs, scs, amts):
    output_dict = {
        "constructor": 0,
        "fields": [
            {
                "list": [
                    {
                        "bytes": key
                    } for key in pkhs
                ]
            },
            {
                "list": [
                    {
                        "bytes": key
                    } for key in scs
                ]
            },
            {
                "list": [
                    {
                        "int": value
                    } for value in amts
                ]
            }
        ]
    }
    return json.dumps(output_dict)

with open("payout.json", "r") as f:
    input_dict = json.load(f)

pkh = []
sc  = []
amt = []

for entry in input_dict:
    addr = entry
    amount = input_dict[entry]
    result = subprocess.run(['./bech32'], input=addr.encode(), stdout=subprocess.PIPE)
    output = result.stdout.decode().strip()
    pkh.append(output[2:58])
    sc.append(output[58:])
    amt.append(amount)

output = json_convert(pkh, sc, amt)

with open("data/datum/payout-datum.json", "w") as f:
    f.write(output)