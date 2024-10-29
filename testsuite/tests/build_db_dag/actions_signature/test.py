import glob
import json
import random
import os.path
import re

from testsuite_support.builder_and_runner import BuilderAndRunner

bnr = BuilderAndRunner()

def read_json_file(jfile):
    with open(jfile, "r") as read_file:
        data = json.load(read_file)
    return data


def write_json_file(jfile, data):
    with open(jfile, "w") as write_file:
        json.dump(data, write_file)


def write_empty_file(file):
    open(file, "w").close()


def write_file(file, data):
    with open(file, "w") as write_file:
        write_file.write(data)


def get_all_signature_json_files(project):
    json_file_list = []
    for filename in glob.glob(f"tree/obj/{project}/.*.json", recursive=True):
        json_file_list.append(filename)
    return json_file_list


def get_json_file(name, project):
    jf_list = get_all_signature_json_files(project)
    if jf_list:
        for fname in jf_list:
            if re.search(name, os.path.basename(fname)):
                return fname
    assert False, f"could not find a json file matching {name}"


def valid_json_fname(name):
    return name is not None and os.path.isfile(name)


def empty_json_file(name, project):
    fname = get_json_file(name, project)
    if valid_json_fname(fname):
        write_empty_file(fname)


def create_json_dict(name, kind, project):
    data = {}
    format_kind = "json"
    if kind == "empty":
        pass
    elif kind == "random":
        data = {"bla": 123, "foo": [{"bar": "foobar"}, {"foo": "barfoo"}]}
    elif kind == "invalid_signature_1":
        data = {"signature": "foobar"}
    elif kind == "invalid_signature_2":
        data = {"signature": [123, 456]}
    elif kind == "invalid_signature_3":
        data = {"signature": [{"uri": "not_an_uri"}]}
    elif kind == "invalid_signature_4":
        data = {"signature": [{"uri": "file://foo.txt", "checksum": "bad_checksum"}]}
    elif kind == "invalid_signature_5":
        data = {"signature": [{"unexpected": 123}]}
    elif kind == "invalid_signature_6":
        data = {"signature": [{123: 123}]}

    fname = get_json_file(name, project)
    if valid_json_fname(fname):
        if format_kind == "json":
            write_json_file(fname, data)
        elif format_kind == "text":
            write_file(fname, data)


def overwrite_artifact_element(name, kind, project):
    fname = get_json_file(name, project)
    if valid_json_fname(fname):
        found = False
        data = read_json_file (fname)

        for item in data["signature"]:
            if item["uri"].endswith(name):
                item["uri"] = "deadcaferand0m"
                print(f"signature for {item['uri']} replaced")
                found = True
        write_json_file(fname, data)
        if not found:
            print(f"cannot find artifact '{name}'")
    else:
        print(f"cannot find a signature file for '{project}'")

def overwrite_checksum(name, ext, checksum, project):
    jf_list = None
    if name != "":
        fname = get_json_file(name, project)
        jf_list = [fname]
    else:
        jf_list = get_all_signature_json_files(project)

    assert jf_list, f"cannot determine the json inputs"
    for fname in jf_list:
        assert valid_json_fname(fname), f"not a valid json {fname}"
        replaced = False
        data = read_json_file (fname)
        for item in data["signature"]:
            if (ext != "" and item["uri"].endswith(ext)) or ext == "":
                item["checksum"] = checksum
                replaced = True
                break
        assert replaced, f"could not replace '{ext}'"
        write_json_file(fname, data)


# ensure .ali and .o files are there for the trees used to test, together
# with gpr2's signatures
bnr.simple_run(["gpr2build", "-Ptree/p1.gpr", "-p", "-q"])
# base1 is extended by p2: ensure it has ali and object files on its own to
# demonstrate that they're inherited
bnr.simple_run(["gpr2build", "-Ptree/base1.gpr", "-p", "-q"])
bnr.simple_run(["gpr2build", "-Ptree/p2.gpr", "-p", "-q"])

# now build and run the test (using bnr.build and bnr.run to have proper
# coverage when requested).
bnr.build(project="test.gpr", args=["-p", "-q"])

MAIN_COMP = r"compile_main.adb.*"

print("================================================================")
print("Case 1 - Initial DAG without changes")
bnr.call(['./main'])
print("================================================================")
print("Case 2 - Altered DAG with empty JSON dictionnary (p1: main)")
create_json_dict(MAIN_COMP, "empty", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 3 - Altered DAG with empty JSON file (p1: main)")
empty_json_file(MAIN_COMP, "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 4a - Altered DAG with random JSON dictionnary (p1: main)")
create_json_dict(MAIN_COMP, "random", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 4b - Altered DAG with corrupted signature (1) (p1: main)")
create_json_dict(MAIN_COMP, "invalid_signature_1", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 4c - Altered DAG with corrupted signature (2) (p1: main)")
create_json_dict(MAIN_COMP, "invalid_signature_2", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 4d - Altered DAG with corrupted signature (3) (p1: main)")
create_json_dict(MAIN_COMP, "invalid_signature_3", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 4e - Altered DAG with corrupted signature (4) (p1: main)")
create_json_dict(MAIN_COMP, "invalid_signature_4", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 4f - Altered DAG with corrupted signature (5) (p1: main)")
create_json_dict(MAIN_COMP, "invalid_signature_5", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 4g - Altered DAG with corrupted signature (6) (p1: main)")
create_json_dict(MAIN_COMP, "invalid_signature_6", "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 5 - Altered DAG with modified checksums (p1: main)")
overwrite_checksum(MAIN_COMP, "", "X"*10, "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 6 - Altered DAG with single modified checksum (p1: main)")
overwrite_checksum(MAIN_COMP, ".o", "X"*40, "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 7 - Altered DAG with modified checksums (p2: pkg3 - pkg)")
overwrite_checksum(r".*pkg3.adb.*", "", "X"*40, "p2")
overwrite_checksum(r".*pkg.adb.*", ".adb", "X"*40, "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 8 - Altered DAG with shorter checksum (p1: main)")
overwrite_checksum(MAIN_COMP, "", "X"*39, "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 9 - Altered DAG with longer checksum (p1: main)")
overwrite_checksum(MAIN_COMP, "", "X"*41, "p1")
bnr.call(['./main'])
print("================================================================")
print("Case 10 - Altered DAG with modified checksums (all)")
overwrite_checksum("", "", "X"*40, "*")
bnr.call(['./main'])
print("================================================================")
print("Case 11 - random artifact element (p1: main)")
overwrite_artifact_element (MAIN_COMP, "random", "p1")
bnr.call(['./main'])
print("================================================================")
