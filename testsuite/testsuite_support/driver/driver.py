from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.driver.driver_db import SCN_ATTRIBUTE_TEST_CONFIG
from testsuite_support.driver.driver_constants import ObjOptions as opt
from testsuite_support.driver.driver_constants import ObjScnAttrValues as oav
from testsuite_support.driver.driver_constants import ObjScnRes as osr
from testsuite_support.driver.driver_constants import ObjScnPhase as osp
from testsuite_support.driver.driver_constants import ObjScnTool as ost
from testsuite_support.driver.driver_constants import ObjScnCmd as osc
from testsuite_support.driver.driver_constants import ObjScnAttrSubstPattern as osasp
from testsuite_support.driver.driver_constants import ObjOptions
from testsuite_support.driver.driver_constants import SCN_TOOLS_CMD
import re
import logging
import mmap
import os
import shlex

bnr = BuilderAndRunner()


class ObjRes:
    def __init__(self, o):
        self.output = None
        self.res = None
        logging.debug(f"Output type = {type(o)}")
        if type(o) == list:
            self.output = o
        elif type(o) == str:
            with open(o, "r") as fp:
                self.output = [line for line in fp]
        else:
            logging.error("Unexpected output format")

    def get(self):
        return self.res

    def log(self):
        logging.debug(f"{self.output}")

    def __compute__(self, d, b, k, vk, v, o):

        def check_expected_switch(cmd_s, s):
            if '=' in s or ' ' in s:
                s, n = re.split("[= ]", s)
            else:
                s = s
                n = None
            if n:
                r = [s in s__ for s_ in cmd_s for s__ in s_]
                if any(r):
                    q = [n in s__ for s_ in cmd_s for s__ in s_]
                index_r = [i for i, x_ in enumerate(r) if x_]
                index_q = [i for i, x_ in enumerate(q) if x_]
                r = [x_ - 1 in index_r for x_ in index_q]
                return any(r)
            else:
                r = [s in s__ for s_ in cmd_s for s__ in s_]
                return any(r)

        def substitute(s, val):
            a_val_prefix, a_val_name, a_val_suffix = val[0]
            if osasp.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value in s:
                return s.replace(osasp.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value,
                                 a_val_prefix + a_val_name + a_val_suffix)
            elif osasp.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value in s:
                return s.replace(osasp.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value,
                                 a_val_prefix)
            elif osasp.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value in s:
                return s.replace(osasp.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value,
                                 a_val_name)
            elif osasp.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value in s:
                return s.replace(osasp.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value,
                                 a_val_suffix)

        behavior = list(map(lambda x: x, b[k]))

        if opt.SCN_OPTION_USE_ALT_ATTR_VALUE in o:
            if vk == oav.SCN_ATTR_VALUES_UNIQUE:
                behavior = list(map(
                    lambda x: substitute(x, v), behavior
                ))
            elif vk == oav.SCN_ATTR_VALUES_CONCATENATED \
                    or vk == oav.SCN_ATTR_VALUES_DISTRIBUTED:
                logging.error("### TODO ###")
                # for i, elt in enumerate(behavior):
                #    if osasp.SCN_ATTR_SUBSTITUTE_PATTERN in elt:
                #        for av in v:
                #            behavior.append(
                #                elt.replace(osasp.SCN_ATTR_SUBSTITUTE_PATTERN.value,
                #                            av)
                #            )
                #            del behavior[i]
        else:
            if vk == oav.SCN_ATTR_VALUES_UNIQUE and v is not None:
                behavior = list(map(
                    lambda x: x.replace(osasp.SCN_ATTR_SUBSTITUTE_PATTERN.value,
                                        v[0]),
                    behavior
                ))
            elif vk == oav.SCN_ATTR_VALUES_CONCATENATED \
                    or vk == oav.SCN_ATTR_VALUES_DISTRIBUTED:
                logging.error("### TODO ###")
                # for i, elt in enumerate(behavior):
                #    if osasp.SCN_ATTR_SUBSTITUTE_PATTERN.value in elt:
                #        for av in v:
                #            behavior.append(
                #                elt.replace(osasp.SCN_ATTR_SUBSTITUTE_PATTERN.value,
                #                            av)
                #            )
                #            del behavior[i]

        cls = []
        for line in self.output:
            if d in line:
                cls.append(line)

        logging.debug(f"{cls}")
        logging.info(f"Expects - [ {k.name} - {behavior} ]")

        res = []
        if vk == oav.SCN_ATTR_VALUES_UNIQUE or oav.SCN_ATTR_VALUES_CONCATENATED:
            for cl in cls:
                cmd_switches = [s.split("=") for s in shlex.split(cl)]
                logging.debug(f"{cmd_switches}")
                sub_res = [check_expected_switch(cmd_switches, s) for s in behavior]
                res.append(sub_res)
            logging.debug(f"Res : {res}")
            if k == osr.SCN_RES_FOR_NONE:
                if self.res is None:
                    self.res = not any(any(r) for r in res)
                    if not self.res:
                        logging.error(f"{behavior} behavior found at least one time"
                                      + " in the command line output")
                else:
                    condition = not any(any(r) for r in res)
                    if not condition:
                        logging.error(f"{behavior} behavior found at least one time"
                                      + " in the command line output")
                    self.res = self.res and condition
            elif k == osr.SCN_RES_FOR_ANY:
                if self.res is None:
                    self.res = any(all(r) for r in res)
                    if not self.res:
                        logging.error(f"{behavior} behavior not found at least one time"
                                      + " in the command line output")
                else:
                    condition = any(all(r) for r in res)
                    if not condition:
                        logging.error(f"{behavior} behavior not found at least one time"
                                      + " in the command line output")
                    self.res = self.res and condition
            elif k == osr.SCN_RES_FOR_ALL:
                if self.res is None:
                    self.res = all(all(r) for r in res)
                    if not self.res:
                        logging.error(f"{behavior} behavior not found"
                                      + " in one of the command line output")
                else:
                    condition = all(all(r) for r in res)
                    if not condition:
                        logging.error(f"{behavior} behavior not found"
                                      + " in one of the command line output")
                    self.res = self.res and condition
        elif vk == oav.SCN_ATTR_VALUES_DISTRIBUTED:
            logging.error("### TODO ###")
            self.res = all(res)

        logging.debug(f"{self.res}")

    def compute(self, delimiter, behavior, vk, v, o):
        if not behavior:
            logging.error("This testcase is not expecting anything to happen")
        else:
            for key in behavior.keys():
                self.__compute__(delimiter, behavior, key, vk, v, o)


class ObjProject:
    def __init__(self, file, altvalue):
        self.filename = os.getcwd() + "/" + file
        self.attribute_value = altvalue

    def compile(self, attribute, options):
        if opt.SCN_OPTION_USE_ALT_ATTR_VALUE not in options:
            with open(self.filename, "r+") as fp:
                pack, attr = attribute.split(".")
                data = mmap.mmap(fp.fileno(), 0).read().decode()
                regex = r'^ *(?:package (\w+) is[\w\s]*)?(?:(?!project|package|end))' \
                        + rf'(.*(for|\')[ ]*{attr}.* use (.*);)'
                pattern = re.compile(regex, flags=re.MULTILINE | re.IGNORECASE)
                results = pattern.findall(data)

                for match_p, match_a, x_, value in results:
                    if (match_p == pack) or (match_p == "" and pack == "Project_Level"):
                        self.attribute_value = \
                            [r.group(0).replace('"', '') for r in
                             re.finditer('"(.*?)"', value)]

    def get_attr_value(self):
        return self.attribute_value

    def log(self):
        logging.debug(f"   Attribute -> {self.attribute_value}")


class ObjTestCase:
    def __init__(self, name, file, altvalue, ttype, options):
        self.file = file
        self.ttype = ttype
        self.project = ObjProject(file, altvalue)
        self.res = None
        self.options = ObjOptions(options)
        self.options_list = [flag.name for flag in ObjOptions if flag in self.options]
        self.setup = []
        self.cleanup = []
        if "setup_cmd" in SCN_ATTRIBUTE_TEST_CONFIG[name]:
            for s in SCN_ATTRIBUTE_TEST_CONFIG[name]['setup_cmd']:
                new_setup_cmd = ObjCmd(s)
                self.setup.append(new_setup_cmd)
        if "cleanup_cmd" in SCN_ATTRIBUTE_TEST_CONFIG[name]:
            for c in SCN_ATTRIBUTE_TEST_CONFIG[name]['cleanup_cmd']:
                new_cleanup_cmd = ObjCmd(c)
                self.cleanup.append(new_cleanup_cmd)

    def compile(self, attribute):
        self.project.compile(attribute, self.options)

    def setup_cleanup(self, setup, cleanup):
        self.setup = setup
        self.cleanup = cleanup
        for s in self.setup:
            s.compile()
        for c in self.cleanup:
            c.compile()

    def get_type(self):
        return self.ttype

    def get_opts(self):
        return self.options_list

    def run(self, config):

        for s in self.setup:
            if type(s.get()) is list:
                cmd = list(map(
                    lambda x: x.replace(osasp.SCN_PRJ_SUBSTITUTE_PATTERN.value,
                                        f"-P{self.file}"),
                    s.get()
                ))
                if opt.SCN_OPTION_USE_CGPR in self.options:
                    if ost.SCN_TOOL_GPRBUILD.name in cmd \
                            or ost.SCN_TOOL_GPRCLEAN.name in cmd:
                        cmd.append(f"--config={osc.SCN_CMD_CONFIG_FILE}")
                logging.debug(f"Setting up : {' '.join(cmd)}")
                bnr.run(cmd)
            elif s.get() is not None:
                custom_cmd = s.get()
                logging.debug(f"Setting up : {custom_cmd}")
                custom_cmd()
            else:
                logging.error(f"Could not launch setup cmd : {s.get()}")

        for b in config.behavior_generator():
            index = f"{b.get_tool()}.{b.get_phase()}"
            if index not in SCN_TOOLS_CMD:
                logging.error("No command available for this tool and phase")
            else:
                cmd = list(map(
                    lambda x: x.replace(osasp.SCN_PRJ_SUBSTITUTE_PATTERN.value,
                                        f"-P{self.file}"),
                    SCN_TOOLS_CMD[index]
                ))
                if opt.SCN_OPTION_USE_CGPR in self.options:
                    if ost.SCN_TOOL_GPRBUILD.tool_name() in cmd \
                            or ost.SCN_TOOL_GPRCLEAN.tool_name() in cmd:
                        cmd.append(f"--config={osc.SCN_CMD_CONFIG_FILE.value}")
                logging.info(f"Launching : \"{' '.join(cmd)}\"")
                out = bnr.run(cmd)
                if any(osc.SCN_CMD_OUTPUT_FILE.value in s for s in cmd):
                    logging.debug("Checking output from "
                                  + f"{osc.SCN_CMD_OUTPUT_FILE.value}")
                    new_output = ObjRes(osc.SCN_CMD_OUTPUT_FILE.value)
                    self.res = new_output
                else:
                    logging.debug("Checking output directly from bnr.run()")
                    new_output = ObjRes(out.out.split("\n"))
                    self.res = new_output

            self.res.log()
            self.res.compute(b.get_delimiter(),
                             b.get_expected_behavior(self.ttype),
                             config.get_value_kind(),
                             self.project.get_attr_value(),
                             self.options)

            if self.res.get() is None:
                logging.info("No result available, something went wrong !")
            elif self.res.get():
                logging.info("Test OK !")
            else:
                logging.info("Test KO !")

        for c in self.cleanup:
            if type(c.get()) is list:
                cmd = list(map(
                    lambda x: x.replace(osasp.SCN_PRJ_SUBSTITUTE_PATTERN.value,
                                        f"-P{self.file}"),
                    c.get()
                ))
                if opt.SCN_OPTION_USE_CGPR in self.options:
                    if ost.SCN_TOOL_GPRBUILD.name in cmd \
                            or ost.SCN_TOOL_GPRCLEAN.name in cmd:
                        cmd.append(f"--config={osc.SCN_CMD_CONFIG_FILE.name}")
                logging.debug(f"Cleaning up : {' '.join(cmd)}")
                bnr.run(cmd)
            elif c.get() is not None:
                custom_cmd = c.get()
                logging.debug(f"Cleaning up : {custom_cmd}")
                custom_cmd()
            else:
                logging.error(f"Could not launch clean up cmd : {c.get()}")

    def log(self):
        logging.debug("[ Testcase ]")
        logging.debug(f"   Kind      -> {self.ttype.name}")
        logging.debug(f"   File      -> {self.file}")
        logging.debug(f"   Options   -> {self.options_list}")
        self.project.log()
        logging.debug("   Setup :")
        for s in self.setup:
            s.log()
        logging.debug("   Cleanup :")
        for c in self.cleanup:
            c.log()


class ObjBehavior:
    def __init__(self, behavior):
        self.tool = behavior['tool']
        if "phase" not in behavior:
            self.phase = osp.SCN_PHASE_NONE
        else:
            self.phase = behavior['phase']
        self.delimiter = behavior['output_delimiter']
        self.has_behavior = False
        self.scn_case_value = {}
        if "expected_behavior" in behavior:
            self.has_behavior = True
            for b in behavior['expected_behavior']:
                self.scn_case_value[b] = behavior['expected_behavior'][b]

    def get_tool(self):
        return self.tool

    def get_phase(self):
        return self.phase

    def get_expected_behavior(self, kind):
        if kind in self.scn_case_value:
            return self.scn_case_value[kind]

    def get_delimiter(self):
        return self.delimiter

    def log(self, i):
        logging.debug(f"[ Behavior - {i} ]")
        logging.debug(f"   Tool      -> {self.tool.name}")
        logging.debug(f"   Phase     -> {self.phase.name}")
        logging.debug(f"   Delimiter -> \"{self.delimiter}\"")
        if self.has_behavior:
            for b, k in self.scn_case_value.items():
                logging.debug(f"   {b.name} :")
                for kk, kv in k.items():
                    logging.debug(f"      - {kk.name} : {kv}")
        else:
            logging.debug("      - No behavior")


class ObjCmd:
    def __init__(self, cmd):
        self.cmd = {}
        self.index = None
        self.tool = cmd['tool']
        if "phase" not in cmd:
            phase = osp.SCN_PHASE_NONE
        else:
            phase = cmd['phase']
        self.phase = phase
        self.index = f"{self.tool}.{self.phase}"

        if self.index in SCN_TOOLS_CMD:
            self.cmd[self.index] = SCN_TOOLS_CMD[self.index]

    def compile(self):
        self.cmd[self.index] = SCN_TOOLS_CMD[self.index]

    def get(self):
        if self.index in self.cmd:
            return self.cmd[self.index]

    def log(self):
        if self.index in self.cmd:
            logging.debug(f"   - [ {self.tool.name} - {self.phase.name} ] "
                          + f": {self.cmd[self.index]}")
        else:
            logging.error(f"   - [ {self.tool.name} - {self.phase.name} ] "
                          + "index does not exist")


class ObjConfig:
    def __init__(self, name):
        self.name = name
        self.value_kind = None
        self.behaviors = []
        if self.name in SCN_ATTRIBUTE_TEST_CONFIG:
            self.value_kind = SCN_ATTRIBUTE_TEST_CONFIG[self.name]['value_kind']
            for b in SCN_ATTRIBUTE_TEST_CONFIG[self.name]['test_cmd']:
                new_behavior = ObjBehavior(b)
                self.behaviors.append(new_behavior)

    def get_name(self):
        return self.name

    def get_index(self):
        p, a = self.name.split(".")
        return f"{p}.{a}"

    def behavior_generator(self):
        for b in self.behaviors:
            yield b

    def get_value_kind(self):
        return self.value_kind

    def log(self):
        logging.debug(f"[ Config ] Name -> {self.name} ")
        logging.debug(f"[ Config ] Kind -> {self.value_kind.name}")
        for i, b in enumerate(self.behaviors, start=1):
            b.log(i)


class ObjScn:
    def __init__(self, attribute):
        self.config = None
        self.testcases = []
        self.has_testcase = False
        if "." not in attribute:
            self.config = ObjConfig(f"Project_Level.{attribute}")
        else:
            self.config = ObjConfig(attribute)
        logging.info(f"Creating a test for {self.config.get_name()} attribute")
        self.config.log()

    def add_testcase(self, file, ttype, altvalue=None, options=0):
        my_testcase = ObjTestCase(self.config.get_name(),
                                  file,
                                  altvalue,
                                  ttype,
                                  options)
        my_testcase.compile(self.config.get_name())
        self.testcases.append(my_testcase)
        self.has_testcase = True
        my_testcase.log()

    def run(self):
        logging.info(f"Running {self.config.get_name()} attribute test")
        for i, case in enumerate(self.testcases, start=1):
            logging.info(f"[ Testcase {i} - {case.get_type().name}"
                         + f" - {case.get_opts()} ]")
            case.run(self.config)

    def has_testcase(self):
        return self.has_testcase
