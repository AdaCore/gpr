from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.tools import GPRBUILD, GPRCLEAN
import re
import sys 

bnr = BuilderAndRunner()

def normalize_gpr_warnings(text):
    lines = text.strip().split('\n')
    result = []
    
    # Track the first "depends on" line to extract lib names for substitution
    lib_map = {}
    lib_counter = [1]
    
    def get_lib_alias(lib_name):
        if lib_name not in lib_map:
            lib_map[lib_name] = f"lib{lib_counter[0]}"
            lib_counter[0] += 1
        return lib_map[lib_name]
    
    gpr_counter = [1]
    gpr_map = {}
    
    def get_gpr_alias(gpr_name):
        # First .gpr is always shown as *.gpr (the "header" warning)
        if gpr_name not in gpr_map:
            gpr_map[gpr_name] = f"{gpr_counter[0]}.gpr"
            gpr_counter[0] += 1
        return gpr_map[gpr_name]
    
    for line in lines:
        # Match: <something>.gpr: warning: circular library dependency detected
        m = re.match(r'(\S+\.gpr): (warning: circular library dependency detected)', line)
        if m:
            result.append(f"*.gpr: {m.group(2)}")
            continue
        
        # Match: <something>.gpr: warning: <libX>.a depends on <libY>.a
        m = re.match(r'(\S+\.gpr): (warning: )(\S+\.a) depends on (\S+\.a)', line)
        if m:
            gpr, warn, lib1, lib2 = m.group(1), m.group(2), m.group(3), m.group(4)
            gpr_alias = get_gpr_alias(gpr)
            lib1_alias = get_lib_alias(lib1)
            lib2_alias = get_lib_alias(lib2)
            result.append(f"{gpr_alias}: {warn}<{lib1_alias}>.a depends on <{lib2_alias}>.a")
            continue
        
        result.append(line)  # Pass through unrecognized lines unchanged
    
    return '\n'.join(result)

def run(cmd):
    print("$ " + " ".join(cmd))
    output = bnr.simple_run(cmd).out

    print(normalize_gpr_warnings(output))

run([GPRBUILD, "-q", "-P", "ie_agg.gpr"])
