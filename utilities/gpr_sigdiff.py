import json
import os.path
import sys

if len(sys.argv) != 3:
    print(f"Usage: {os.path.basename(sys.argv[0])} sig1.json sig2.json")
    exit


def read(fname):
    res = {}

    with open(fname) as fp:
        cnt = json.load(fp)

    for val in cnt["inputs"]:
        if val["key"] in res:
            print(f"!!! duplicated key in {fname}: {res[val['key']][1]} - {val['key']}")
            exit
        res[val["key"]] = [val["value"], val["class"]]

    return res


def check_keys(fname1, dict1, fname2, dict2):
    if len(dict1) == len(dict2) and dict1.keys == dict2.keys:
        return True

    unique1 = [key for key in dict1.keys() if key not in dict2]
    unique2 = [key for key in dict2.keys() if key not in dict1]
    for key in unique1:
        print(f"only in {fname1}: {key}")
    for key in unique2:
        print(f"only in {fname2}: {key}")
    return False


fname1 = sys.argv[1]
fname2 = sys.argv[2]

sig1 = read(fname1)
sig2 = read(fname2)

if not check_keys(fname1, sig1, fname2, sig2):
    exit

for k in sig1.keys():
    if sig1[k][0] != sig2[k][0]:
        print(f"values differ for key {k}:")

        if k == "command_line":
            args1 = sig1[k][0].split(" ")
            args2 = sig2[k][0].split(" ")
            length = max(len(args1), len(args2))

            for j in range(0, length):
                if j >= len(args1):
                    print(f" !!{j} - '' <> {args2[j]}")
                elif j >= len(args2):
                    print(f" !!{j} - {args1[j]} <> ''")
                elif args1[j] != args2[j]:
                    print(f" !!{j} - {args1[j]} <> {args2[j]}")
                else:
                    print(f"   {j} - {args1[j]}")
        else:
            print(f" - {sig1[k][0]}")
            print(f" - {sig2[k][0]}")
