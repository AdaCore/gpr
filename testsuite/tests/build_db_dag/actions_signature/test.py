import glob
import json
import random
import os.path

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


def get_all_json_file():
	json_file_list = []
	for filename in glob.glob('tree/obj/*/*.json', recursive=True):
		json_file_list.append(filename)
	return json_file_list


def get_json_file(name):
	jf_list = get_all_json_file()
	if jf_list:
		for fname in jf_list:
			if name in fname:
				return fname
	return "none"


def valid_json_fname(name):
	if os.path.isfile(name):
		return True
	return False


def empty_json_file(name):
	fname = get_json_file(name)
	if valid_json_fname(fname):
		write_empty_file(fname)


def create_json_dict(name, kind):
	data = {}
	format_kind = "json"
	if kind == "empty":
		pass
	elif kind == "random":
		data = {"bla": "blabla", "foo": [{"bar": "foobar"}, {"foo": "barfoo"}]} 
	elif kind == "invalid":
		data = '{"artifacts": [{"foo": "bar" "bla": "bla"}]}'
		format_kind = "text"

	fname = get_json_file(name)
	if valid_json_fname(fname):
		if format_kind == "json":
			write_json_file(fname, data)
		elif format_kind == "text":
			write_file(fname, data)


def overwrite_artifact_element(name, kind):
	fname = get_json_file(name)
	if valid_json_fname(fname):
		data = read_json_file (fname)

		element = {}
		if kind == "random":
			element = {"bar": "foobar"}
		elif kind == "duplicate":
			element = data["artifacts"][0]

		data["artifacts"].append(element)
		write_json_file(fname, data)


def overwrite_checksum(name, ext, checksum):
	if name != "":
		fname = get_json_file(name)
		if valid_json_fname(fname):
			data = read_json_file (fname)
			for index,artifact in enumerate(data["artifacts"]):
				if ext != "" and artifact["plain_id"].endswith(ext):
					data["artifacts"][index]["checksum"] = checksum
				elif ext == "":
					data["artifacts"][index]["checksum"] = checksum
			write_json_file(fname, data)
	else:
		jf_list = get_all_json_file()
		if jf_list:
			for fname in jf_list:
				data = read_json_file (fname)
				for index,artifact in enumerate(data["artifacts"]):
					if ext != "" and artifact["plain_id"].endswith(ext):
						data["artifacts"][index]["checksum"] = checksum
					elif ext == "":
						data["artifacts"][index]["checksum"] = checksum
				write_json_file(fname, data)


# ensure .ali and .o files are there for the trees used to test
bnr.call(["gprbuild", "-Ptree/p1.gpr", "-p", "-q"])
# base1 is extended by p2: ensure it has ali and object files on its own to
# demonstrate that they're inherited
bnr.call(["gprbuild", "-Ptree/base1.gpr", "-p", "-q"])
bnr.call(["gprbuild", "-Ptree/p2.gpr", "-p", "-q"])

# now build and run the test (using bnr.build and bnr.run to have proper
# coverage when requested).
bnr.build(project="test.gpr", args=["-p", "-q"])

print("================================================================")
print("Case 1 - Initial DAG")
bnr.call(['./main'])
print("================================================================")
print("Case 2 - Build DAG without changes")
bnr.call(['./main'])
print("================================================================")
print("Case 3 - Altered DAG with empty JSON dictionnary (main_p1)")
create_json_dict("main_p1", "empty")
bnr.call(['./main'])
print("================================================================")
print("Case 4 - Altered DAG with empty JSON file (main_p1)")
empty_json_file("main_p1")
bnr.call(['./main'])
print("================================================================")
print("Case 5 - Altered DAG with random JSON dictionnary (main_p1)")
create_json_dict("main_p1", "random")
bnr.call(['./main'])
print("================================================================")
print("Case 6 - Altered DAG with modified checksums (main_p1)")
overwrite_checksum("main_p1", "", "X"*40)
bnr.call(['./main'])
print("================================================================")
print("Case 7 - Altered DAG with single modified checksum (main_p1)")
overwrite_checksum("main_p1", ".o", "X"*40)
bnr.call(['./main'])
print("================================================================")
print("Case 8 - Altered DAG with modified checksums (pkg3_p2 - pkg_p1)")
overwrite_checksum("pkg3_p2", "", "X"*40)
overwrite_checksum("pkg_p1", ".adb", "X"*40)
bnr.call(['./main'])
print("================================================================")
print("Case 9 - Altered DAG with shorter checksum (main_p1)")
overwrite_checksum("main_p1", "", "X"*39)
bnr.call(['./main'])
print("================================================================")
print("Case 10 - Altered DAG with longer checksum (main_p1)")
overwrite_checksum("main_p1", "", "X"*41)
bnr.call(['./main'])
print("================================================================")
print("Case 11 - Altered DAG with modified checksums (all)")
overwrite_checksum("", "", "X"*40)
bnr.call(['./main'])
print("================================================================")
print("Case 12 - Duplicated artifact (main_p1)")
overwrite_artifact_element ("main_p1", "duplicate")
bnr.call(['./main'])
print("================================================================")
print("Case 13 - random artifact element (main_p1)")
overwrite_artifact_element ("main_p1", "random")
bnr.call(['./main'])
print("================================================================")
print("Case 14 - wrong JSON format (main_p1)")
create_json_dict ("main_p1", "invalid")
bnr.call(['./main'])
print("================================================================")
