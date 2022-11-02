#!/usr/bin/env python3

import argparse
import importlib.util
import math
import os
import shutil
import sys
import testsuite_support.driver.driver_constants as consts

driver_tests_dir = os.path.dirname(__file__) + "/tests/tooling/"
max_len = 80


def show_doc():

    def pprint(string):
        while len(string) > 0:
            length = min(len(string), max_len - 1)
            if length == max_len - 1:
                length = min(string[:length].rfind(" "), max_len - 1)
            print(f"| {string[:length]}" + (
                        max_len - len(string[:length]) - 1) * " " + "|")
            string = string[length + 1:]

    title = "Test Driver Documentation"
    print("+" + max_len * "=" + "+")
    print("|" + math.floor((max_len - len(title)) / 2) * " " + title
          + math.ceil((max_len - len(title)) / 2) * " " + "|")
    print("+" + max_len * "=" + "+")
    pprint("This test driver is based on a descriptive scenario of which commands is "
           + "going to be launched and the expected result to look for in the output.")
    pprint("Most of the work is to properly define the scenario description. In order "
           + "to do so you will find below every parameters and their definitions to "
           + "properly adjust your test to your needs.")
    pprint("Alternative tool commands can be added to driver_constants.SCN_TOOLS_CMD "
           + "to properly match what you need.")
    print("+" + max_len * "=" + "+")

    pprint("ObjScnAttrValues (Value) : Defines how is the attribute is influencing the "
           + "output of a test.")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjScnAttrValues:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")

    pprint("ObjScnTool (Tool) : Tool that will be launch for the setup, test and "
           + "cleanup phase of the test. Generally it is followed by a specific phase "
           + "for a more precise action to be done.Otherwise it will launch the "
           + "generic command available.")
    pprint("All existing Tool and command can be found in the "
           + "driver_constants.SCN_TOOLS_CMD matrix.")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjScnTool:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")

    pprint("ObjScnPhase (Phase) : Phase of the tool that will be launched. Generally "
           + "each tool have their own set of phases to be more precise on what will "
           + "be executed. This is done to minimize the amount of output to compute ")
    pprint("(E.g.: Testing an attribute that only influence the link phase, "
           + "we are not interested in the compilation apart from it working).")
    pprint("All existing Tool and phase specific commands can be found in the "
           + "driver_constants.SCN_TOOLS_CMD matrix.")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjScnPhase:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")

    pprint("ObjScnOutput (Output) : Defines which kind of output a set of Tool and "
           + "Phase will generate.")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjScnOutput:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")

    pprint("ObjScnCaseValue (Case) : Defines which kind of testcase is being run and "
           + "what output is expected for that testcase.")
    pprint("(E.g. : You can expect different behaviors when the attribute is "
           + "undefined, invalid and defined. The expected output are set in the "
           + "scenario description and retrieved when adding a testcase with the same "
           + "Case).")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjScnCaseValue:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")

    pprint("ObjScnRes (Res) : Defines how your expected results must behave in the"
           + "test output.")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjScnRes:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")

    pprint("ObjScnAttrSubstPattern (Pattern) : Pattern that are changed when running "
           + "a scenario.")
    pprint("(E.g. : only one command is defined but this command can be launched on "
           + "different files due to a pattern that will change the target).")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjScnAttrSubstPattern:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")

    pprint("ObjOptions (Options) : Options that can be set to change precise part of "
           + "the driver behavior. Options can be set scenario wide or testcase wide. "
           + "For each testcase, scenario wide options will be concatenated with the "
           + "testcase ones.")
    print("+" + max_len * "-" + "+")
    for v in consts.ObjOptions:
        pprint(v.doc())
    print("+" + max_len * "=" + "+")


def list_tests(name):
    if not name:
        dirs = os.listdir(driver_tests_dir)
        print("[INFO] Existing tests :")
        for d in dirs:
            if os.path.exists(driver_tests_dir + d + "/test.py"):
                print(f"[INFO] {d}")
    else:
        file_path = driver_tests_dir + name + "/test.py"
        if os.path.exists(file_path):
            with open(file_path, 'r') as file:
                print(f"{''.join([line for line in file])}")
        else:
            print(f"[ERROR] {file_path} does not exists !")


def create_test(name):
    if not name:
        print("[ERROR] Cannot create an unspecified test !")
    else:
        file_path = driver_tests_dir + name + "/test.py"
        if os.path.exists(file_path):
            print(f"[ERROR] {file_path} already exists !")
        else:
            if not os.path.exists(driver_tests_dir + name + "/files"):
                os.makedirs(driver_tests_dir + name + "/files")
            if not os.path.exists(driver_tests_dir + name + "/test.yaml"):
                with open(driver_tests_dir + name + "/test.yaml", 'a') as file:
                    file.write("description:\n    Test XXXXX attribute(s)\n"
                               + "driver: python_script")
            if not os.path.exists(driver_tests_dir + name + "/test.out"):
                with open(driver_tests_dir + name + "/test.out", 'a'):
                    pass
            with open(file_path, 'a') as file:
                file.write("from testsuite_support.driver.driver_imports import "
                           + "create_scenario, add_testcase, run\n")
                file.write("from testsuite_support.driver.driver_imports import "
                           + "edit_custom_command\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjOptions as Opt\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjScnAttrValues as Value\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjScnRes as Res\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjScnCaseValue as Case\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjScnTool as Tool\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjScnPhase as Phase\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjScnAttrSubstPattern as Pattern\n")
                file.write("from testsuite_support.driver.driver_constants import "
                           + "ObjScnExternalTool as ExtTool\n")
                file.write("import logging\n\n")

                file.write('"""\n')
                file.write('This structure describes a test that can be run.\n')
                file.write('Each test has it\'s set of setup and cleanup command, '
                           + 'the direct command which\n')
                file.write('output is monitored to a have a specific fixed '
                           + 'behavior.\n\n')
                file.write('"attributes": Is the list of attributes that are being'
                           + ' tested.\n\n')
                file.write('"value_kind": Is the attribute a simple value '
                           + 'or a list. If it\'s a list\n')
                file.write('are the values meant to be concatenated '
                           + 'or distributed.\n\n')
                file.write('"setup_cmd": common command to every testcase that will'
                           + ' be run before the main\n')
                file.write('command. This set of command will not be '
                           + 'monitored and will not impact the\n')
                file.write('testcase result (except if the'
                           + ' result is directly dependent on it).\n')
                file.write('Custom command can be defined if they are '
                           + ' not (itself or a close variant)\n')
                file.write('present in driver_constants.py. Especially '
                           + 'useful to make small modification\n')
                file.write('between two GPRtools command '
                           + '(modifying an attribute value in a previously\n')
                file.write('created file by a setup command)\n\n')
                file.write('"cleanup_cmd": common command to every testcase that will'
                           + ' be run before the main\n')
                file.write('command. This set of command will not be '
                           + 'monitored and will not impact the\n')
                file.write('testcase result (except if the'
                           + ' result is directly dependent on it).\n')
                file.write('Custom command can be defined if they are '
                           + ' not (itself or a close variant)\n')
                file.write('present in driver_constants.py. Especially useful to '
                           + 'make small modification\n')
                file.write('between two cleanup command (deleting files that could '
                           + 'influence the next testcase)\n\n')
                file.write('"test_cmd": Mains command of the test. Those command are '
                           + 'being monitored and will\n')
                file.write('determine the result of the test.\n\n')
                file.write('- "tool": The tool that will be launched by the '
                           + 'driver.\n\n')
                file.write('- "phase": Which phase of the tool will be launched. '
                           + 'If it is a subcommand of a\n')
                file.write('toolchain, the prior individual commands must be set in'
                           + ' the "setup_cmd".\n')
                file.write('Eg : A test want to monitor GPRbuild Linking phase. '
                           + 'Compilation and Binding\n')
                file.write('must be launched as a setup prior to the link.\n\n')
                file.write('- "output_delimiter": String sequence that will be used '
                           + ' to select a set of lines\n')
                file.write('from main command output to monitor the result in.\n\n')
                file.write('- "expected_behavior": Definition of the results to look'
                           + ' for. Expected behavior\n')
                file.write('can differ based on if the attribute is defined, '
                           + 'undefined, set as default\n')
                file.write('or to an invalid value.\n')
                file.write('For each of those cases it is possible to define what '
                           + 'it is expected to happen.\n')
                file.write('Based on the attribute specificity and its value, you '
                           + 'can expect to find the\n')
                file.write('behavior on every lines of the output, at least one '
                           + 'time, or not at all.\n\n')
                file.write('Please launch "./driver_support.py --doc" for a more '
                           + 'extensive help about usable\nvalues and options.\n')
                file.write('"""\n\n')
                file.write("scn_descr = {\n")
                file.write('    "attributes": ["Package.Attribute"],\n')
                file.write('    "value_kind": Value.SCN_ATTR_VALUES_UNKNOWN,\n')
                file.write('    "setup_cmd": [{"tool": Tool.SCN_TOOL_NONE, '
                           + '"phase": Phase.SCN_PHASE_NONE}],\n')
                file.write('    "test_cmd": [\n')
                file.write('        {\n')
                file.write('            "tool": Tool.SCN_TOOL_NONE, '
                           + '"phase": Phase.SCN_PHASE_NONE,\n')
                file.write('            "output_delimiter": "XXX",\n')
                file.write('            "expected_behavior": {\n')
                file.write('                Case.SCN_CASE_VALUE_DEFAULT: {\n')
                file.write('                    Res.SCN_RES_FOR_EMPTY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_NONE: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ANY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ALL: ["XXX"]\n')
                file.write('                },\n')
                file.write('                Case.SCN_CASE_VALUE_DEF: {\n')
                file.write('                    Res.SCN_RES_FOR_EMPTY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_NONE: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ANY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ALL: ["XXX"]\n')
                file.write('                },\n')
                file.write('                Case.SCN_CASE_VALUE_UNDEF: {\n')
                file.write('                    Res.SCN_RES_FOR_EMPTY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_NONE: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ANY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ALL: ["XXX"]\n')
                file.write('                },\n')
                file.write('                Case.SCN_CASE_VALUE_INVALID: {\n')
                file.write('                    Res.SCN_RES_FOR_EMPTY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_NONE: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ANY: ["XXX"],\n')
                file.write('                    Res.SCN_RES_FOR_ALL: ["XXX"]\n')
                file.write('                }\n')
                file.write('            }\n')
                file.write('        }\n')
                file.write('    ],\n')
                file.write('    "cleanup_cmd": [{"tool": Tool.SCN_TOOL_NONE, '
                           + '"phase": Phase.SCN_PHASE_NONE}],\n')
                file.write('}\n\n')

                file.write("create_scenario(scn_descr, log_level=logging.INFO)\n")
                file.write("run()\n")
            list_tests(name)


def delete_test(name):
    if not name:
        print("[ERROR] Cannot delete an unspecified test !")
    else:
        file_path = driver_tests_dir + name
        if ".." not in name:
            shutil.rmtree(file_path)
        else:
            print(f"[ERROR] Can only delete in {driver_tests_dir} : {file_path}")


def run_test(name):
    if not name:
        print("[ERROR] Cannot run an unspecified test !")
    else:
        file_path = driver_tests_dir + name + "/test.py"
        if os.path.exists(file_path):
            os.chdir(os.path.dirname(file_path))
            spec = importlib.util.spec_from_file_location("test", file_path)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
        else:
            print(f"[ERROR] {file_path} does not exists !")


def main(args=None):
    prog_description = "Test driver support for creating and running tests locally"
    parser = argparse.ArgumentParser(description=prog_description)

    parser.add_argument("-a", "--action", help="Choose what action should be done",
                        choices=["show", "add", "del", "run", "doc"],
                        required=True)
    parser.add_argument("-n", "--name", help="Choose a name on which the action will "
                                             + "be done",
                        required=False)
    try:
        args = vars(parser.parse_args())

        if args['action'] == "doc":
            show_doc()
        elif args["action"] == "show":
            list_tests(args["name"])
        elif args["action"] == "add":
            create_test(args["name"])
        elif args["action"] == "del":
            delete_test(args["name"])
        elif args["action"] == "run":
            run_test(args["name"])

    except argparse.ArgumentError:
        sys.exit(1)


if __name__ == "__main__":
    main()
