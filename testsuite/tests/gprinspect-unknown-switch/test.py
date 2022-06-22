from testsuite_support.builder_and_runner import BuilderAndRunner, GPRINSPECT

bnr = BuilderAndRunner()


def examine_output_unknown_switch(output):
    res = "NONE"
    for o in output.split("\n"):
        if "gprinspect: unrecognized option" in o:
            res = "CONDITION_1"
        if "try \"gprinspect --help\" for more information." in o:
            res = res + "_CONDITION_2"
    if res == "CONDITION_1_CONDITION_2":
        print("OK !")
    else:
        print("KO !")


def examine_output_unknown_switch_value(output):
    res = "NONE"
    for o in output.split("\n"):
        if "gprinspect: use --display=<value> with" in o:
            res = "CONDITION_1"
        if "try \"gprinspect --help\" for more information." in o:
            res = res + "_CONDITION_2"
    if res == "CONDITION_1_CONDITION_2":
        print("OK !")
    else:
        print("KO !")


print("Testcase 1 - unknown switch")
cmd = [GPRINSPECT, '--unknown']
print(f'{" ".join(cmd)}')
r = bnr.run(cmd)
print('Expects : ["gprinspect: unrecognized option"'
      + ', "try \"gprinspect --help\" for more information."]')
examine_output_unknown_switch(r.out)

print("Testcase 2 - unknown display value")
cmd = [GPRINSPECT, '--display=unknown']
print(f'{" ".join(cmd)}')
r = bnr.run(cmd)
print('Expects : ["gprinspect: use --display=<value> with"'
      + ', "try \"gprinspect --help\" for more information."]')
examine_output_unknown_switch_value(r.out)
