import json
from testsuite_support.builder_and_runner import BuilderAndRunner, GPRDOC


bnr = BuilderAndRunner()

p = bnr.run([GPRDOC])

if p.status != 0:
    print("could not run gprdoc")
    exit(1)

# load the output of gprdoc
up_to_date = json.loads(p.out)

with open("attrs.json", "r") as fp:
    doc_variant = json.load(fp)

has_error = False

for item in doc_variant.keys():
    if item not in up_to_date:
        print("documented attribute " & item & " is not defined anymore")
        has_error = True

for item in up_to_date.keys():
    if item not in doc_variant:
        print("attribute " & item & " is not documented")
        has_error = True

if not has_error:
    for item in up_to_date.keys():
        if up_to_date[item] != doc_variant[item]:
            print("attribute " & item & " changed")
            has_error = True

if has_error:
    print("!!! doc needs to be regenerated: do 'make docgen' and commit the changes")
else:
    print("OK, doc up to date")
