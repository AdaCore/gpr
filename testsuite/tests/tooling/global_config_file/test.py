from testsuite_support.builder_and_runner import BuilderAndRunner, GPRCLEAN
import re
import shlex

bnr = BuilderAndRunner()


def get_attribute(gpr_file):
    with open(gpr_file, "r") as fp:
        for line in fp:
            if "Global_Config_File" in line:
                return re.search('use "(.*)";', line).group(1)


def examine_compilation_cl(log_file, expected_switches):
    with open(log_file, "r") as fp:
        def check_expected_switch(switches_l, switch):
            if "=" in switch:
                s, n = switch.split("=")
            else:
                s = switch
                n = None
            res_s = [s in switch for switches in switches_l for switch in switches]
            if any(res_s) and n:
                res_n = [n in switch for switches in switches_l for switch in switches]
                if any(res_n):
                    return True
            if any(res_s) and not n:
                return True
            return False

        def check_compile_line(cl):
            switches_l = [s.split("=") for s in shlex.split(cl)]
            res_s = []
            for expected_switch in expected_switches:
                res_s.append(check_expected_switch(switches_l, expected_switch))
            return all(res_s)

        cls = [line for line in fp if "gcc -c" in line]
        res = [check_compile_line(cl) for cl in cls]
        if all(res):
            print("OK, compilation switches consistent with "
                  + "Global_Config_File attribute !")
        else:
            print("KO, compilation switches inconsistent with "
                  + "Global_Config_File attribute !")


print("Case 1 - no Global_Config_File attribute:")
#############################
cmd = ['gprbuild', '-c', '--build-script=build.log', '-Pfiles/test_no_config.gpr']
compilation_switches = ['-gnatA']
#############################
print(f'{" ".join(cmd)}')
bnr.run(cmd)
examine_compilation_cl("build.log", compilation_switches)
bnr.run([GPRCLEAN, '-p', '-r', '-Pfiles/test_no_config.gpr'])

print("Case 2 - Global_Config_File attribute of an existing file")
#############################
cmd = ['gprbuild', '-c', '--build-script=build.log', '-Pfiles/test_config.gpr']
compilation_switches = ['-gnatA', f'-gnatec={get_attribute("files/test_config.gpr")}']
#############################
bnr.run(cmd)
print(f'{" ".join(cmd)}')
examine_compilation_cl("build.log", compilation_switches)
bnr.run([GPRCLEAN, '-p', '-r', '-Pfiles/test_config.gpr'])

print("Case 3 - Global_Config_File attribute of an unknown file")
#############################
cmd = ['gprbuild', '-c', '--build-script=build.log', '-Pfiles/test_unknown_config.gpr']
compilation_switches = ['-gnatA',
                        f'-gnatec={get_attribute("files/test_unknown_config.gpr")}']
#############################
bnr.run(cmd)
print(f'{" ".join(cmd)}')
examine_compilation_cl("build.log", compilation_switches)
bnr.run([GPRCLEAN, '-p', '-r', '-Pfiles/test_unknown_config.gpr'])
