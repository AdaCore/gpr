import json
from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRDOC


bnr = BuilderAndRunner()

p = bnr.run([GPRDOC])

if Env().host.platform.endswith('windows'):
    is_win = True
else:
    is_win = False


def read(json_struct):
    # read the raw json structure and build a more easy to manipulate
    # structure from it
    result = {}
    for p in json_struct["packages"]:
        pkg_name = p["package_name"]
        if pkg_name == "Project_Level":
            prefix = ""
        else:
            prefix = pkg_name + "."
        for a in p["attributes"]:
            qname = prefix + a["attribute_name"]
            result[qname] = a
    return result


if p.status != 0:
    print("could not run gprdoc")
    exit(1)

# load the output of gprdoc
doc_new = read(json.loads(p.out))

with open("attrs.json", "r") as fp:
    doc_commited = read(json.load(fp))

if is_win:
    # default value for exe_extension is different on windows host.
    # so change the default...
    doc_new["Builder.Executable_Suffix"]["attribute_def"]["default"]["value"] = ""
    # case sensitivity of Interfaces attribute is different on windows
    doc_new["Interfaces"]["attribute_def"]["value_case_sensitive"] = True

has_error = False

for item in doc_commited.keys():
    if item not in doc_new:
        print("documented attribute " + item + " is not defined anymore")
        has_error = True

for item in doc_new.keys():
    if item not in doc_commited:
        print("attribute " + item + " is not documented")
        has_error = True

if not has_error:
    for item in doc_new.keys():
        if doc_new[item] != doc_commited[item]:
            print("attribute " + item + " changed")
            has_error = True

if has_error:
    print("!!! doc needs to be regenerated: do 'make docgen' and commit the changes")
else:
    print("OK, doc up to date")
