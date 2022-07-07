from e3.env import Env
from testsuite_support.builder_and_runner import BuilderAndRunner
from testsuite_support.driver.driver_db import SCN_ATTRIBUTE_TEST_CONFIG
from testsuite_support.driver.driver_constants import ObjOptions as Opt
from testsuite_support.driver.driver_constants import ObjScnPhase as Phase
from testsuite_support.driver.driver_constants import ObjScnCaseValue as Case
from testsuite_support.driver.driver_constants import ObjScnTool as Tool
from testsuite_support.driver.driver_constants import ObjScnCmd as Cmd
from testsuite_support.driver.driver_constants import ObjScnOutput as Output
from testsuite_support.driver.driver_constants import ObjScnAttrSubstPattern as Pattern
from testsuite_support.driver.driver_constants import SCN_TOOLS_CMD
import re
import logging
import mmap
import os
import shlex

bnr = BuilderAndRunner()


class ObjRes:
    def __init__(self, out, k, opts):
        self.output = None
        self.output_kind = k
        self.res = None
        logging.debug(f"Output type = {type(out)}")
        if type(out) == list:
            self.output = out
        elif type(out) == str:
            with open(out, "r") as fp:
                self.output = [line for line in fp]
        else:
            logging.error("Unexpected output format")
        self.__compile__(opts)

    def get(self):
        return self.res

    def log(self):
        logging.debug(f"{self.output}")

    def compute(self, delimiter, behavior, vk, v, o, tct):
        if not behavior:
            logging.error("This testcase is not expecting anything to happen")
        else:
            for key in behavior.keys():
                self.__compute__(delimiter, behavior, key, vk, v, o, tct)

    @staticmethod
    def __check_expected__(cmd_s, s):
        if "=" in s or " " in s:
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

    @staticmethod
    def __check_expected_strict__(cmd_s, s):
        if "=" in s or " " in s:
            s, n = re.split("[= ]", s)
        else:
            s = s
            n = None
        if n:
            r = [s == s__ for s_ in cmd_s for s__ in s_]
            if any(r):
                q = [n == s__ for s_ in cmd_s for s__ in s_]
            index_r = [i for i, x_ in enumerate(r) if x_]
            index_q = [i for i, x_ in enumerate(q) if x_]
            r = [x_ - 1 in index_r for x_ in index_q]
            return any(r)
        else:
            r = [s == s__ for s_ in cmd_s for s__ in s_]
            return any(r)

    @staticmethod
    def __substitute__(s, val):
        a_val_prefix, a_val_name, a_val_suffix = val
        a_val_prefix = os.path.normpath(a_val_prefix)
        a_val_name = os.path.normpath(a_val_name)
        a_val_suffix = os.path.normpath(a_val_suffix)

        if Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value in s:
            return s.replace(
                Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value,
                a_val_prefix + a_val_name + a_val_suffix,
            )
        elif Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value in s:
            return s.replace(
                Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value,
                a_val_prefix + a_val_name,
            )
        elif Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value in s:
            return s.replace(Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value, a_val_name)
        elif Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value in s:
            return s.replace(
                Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value,
                a_val_name + a_val_suffix,
            )

    @staticmethod
    def __substitute_image__(s, val):
        a_val_prefix, a_val_name, a_val_suffix = val
        if Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value in s:
            return s.replace(Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value, a_val_name)
        elif Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value in s:
            return s.replace(
                Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value, a_val_name
            )
        elif Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value in s:
            return s.replace(Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value, a_val_name)
        elif Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value in s:
            return s.replace(
                Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value, a_val_name
            )

    @staticmethod
    def __pattern_defined__(elt):
        c = Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value in elt
        c = c or Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value in elt
        c = c or Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value in elt
        c = c or Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value in elt
        return c

    @staticmethod
    def __transpose__(result):
        return list(map(list, zip(*result)))

    def __compile__(self, opts):
        if self.output is not None:
            if self.output_kind is not None:
                for i, line in enumerate(self.output):
                    if line == "":
                        del self.output[i]
                if self.output_kind == Output.SCN_OUTPUT_FILE_LIST:
                    if Opt.SCN_OPTION_CONVERT_FILE_TO_DIRNAME in opts:
                        self.output = list(
                            map(lambda x: x.replace(x, os.path.dirname(x)), self.output)
                        )
            else:
                logging.error("Attempt to exploit an output which is not defined")

    def __compute__(self, d, b, k, vk, v, o, tct):

        behavior = list(map(lambda x: x, b[k]))
        behavior_image = behavior.copy()

        if Opt.SCN_OPTION_USE_ALT_ATTR_VALUE in o:
            if k in v:
                if vk.unique():
                    behavior = list(
                        map(lambda x: self.__substitute__(x, v[k][0]), behavior)
                    )
                elif vk.concatenated():
                    values = " ".join(v[k])
                    behavior = list(
                        map(lambda x: self.__substitute__(x, values), behavior)
                    )
                elif vk.distributed():
                    for i, elt in enumerate(behavior):
                        if self.__pattern_defined__(elt):
                            for av in v[k]:
                                behavior.append(self.__substitute__(elt, av))
                            del behavior[i]
            else:
                for i, elt in enumerate(behavior):
                    if self.__pattern_defined__(elt):
                        del behavior[i]
        else:
            if vk.unique() and v is not None:
                behavior = list(
                    map(
                        lambda x: x.replace(
                            Pattern.SCN_ATTR_SUBSTITUTE_PATTERN.value, v[0]
                        ),
                        behavior,
                    )
                )
            elif vk.concatenated():
                values = " ".join(v)
                behavior = list(
                    map(
                        lambda x: x.replace(
                            Pattern.SCN_ATTR_SUBSTITUTE_PATTERN.value, values
                        ),
                        behavior,
                    )
                )
            elif vk.distributed():
                for i, elt in enumerate(behavior):
                    if (
                        Pattern.SCN_ATTR_ALT_SUBST_PATTERN_ALL.value in elt
                        or Pattern.SCN_ATTR_ALT_SUBST_PATTERN_PREFIX.value in elt
                        or Pattern.SCN_ATTR_ALT_SUBST_PATTERN_NAME.value in elt
                        or Pattern.SCN_ATTR_ALT_SUBST_PATTERN_SUFFIX.value in elt
                    ):
                        for av in v:
                            behavior.append(
                                elt.replace(
                                    Pattern.SCN_ATTR_SUBSTITUTE_PATTERN.value, av
                                )
                            )
                        del behavior[i]

        if Opt.SCN_OPTION_HIDE_PATH_TO_FILE in o:
            if Opt.SCN_OPTION_USE_ALT_ATTR_VALUE in o:
                if k in v:
                    if vk.unique():
                        behavior_image = list(
                            map(
                                lambda x: self.__substitute_image__(x, v[k][0]),
                                behavior_image,
                            )
                        )
                    elif vk.concatenated():
                        values = " ".join(v[k])
                        behavior_image = list(
                            map(
                                lambda x: self.__substitute_image__(x, v[k][0]),
                                behavior_image,
                            )
                        )
                    elif vk.distributed():
                        for i, elt in enumerate(behavior_image):
                            if self.__pattern_defined__(elt):
                                for av in v[k]:
                                    behavior_image.append(
                                        self.__substitute_image__(elt, av)
                                    )
                                del behavior_image[i]
                else:
                    for i, elt in enumerate(behavior_image):
                        if self.__pattern_defined__(elt):
                            del behavior_image[i]
            else:
                behavior_image = behavior.copy()
        else:
            behavior_image = behavior.copy()

        cls = []
        for line in self.output:
            if d in line:
                cls.append(line)

        logging.debug(f"{cls}")
        if tct == Case.SCN_CASE_VALUE_DEFAULT:
            logging.debug(f"Expects - [ {k.name} - {behavior_image} ]")
        else:
            logging.info(f"Expects - [ {k.name} - {behavior_image} ]")

        result = []
        if behavior:
            for cl in cls:
                if Opt.SCN_OPTION_RES_AS_A_FULL_STRING in o:
                    sub_res = [s in cl for s in behavior]
                else:
                    if Env().host.platform.endswith("windows"):
                        cmd_switches = [
                            re.split("[= ]", s) for s in shlex.split(cl, posix=False)
                        ]
                    else:
                        cmd_switches = [re.split("[= ]", s) for s in shlex.split(cl)]
                    logging.debug(f"{cmd_switches}")
                    if Opt.SCN_OPTION_FULL_STRING_COMPARISION in o:
                        sub_res = [
                            self.__check_expected_strict__(cmd_switches, s)
                            for s in behavior
                        ]
                    else:
                        sub_res = [
                            self.__check_expected__(cmd_switches, s) for s in behavior
                        ]
                result.append(sub_res)
        else:
            if k.none():
                result.append([False])
            else:
                result.append([True])

        if vk.distributed():
            result = self.__transpose__(result)
        logging.debug(f"Checking result : {result}")

        if k.none():
            if not self.output:
                self.res = False
                logging.error("No output to check the results in")
            else:
                if self.res is None:
                    self.res = not any(any(r) for r in result)
                    logging.debug(f"Testcase contribution : {self.res}")
                    if not self.res:
                        logging.error(
                            f"{behavior_image} behavior found at least one"
                            + " time in the output"
                        )
                else:
                    condition = not any(any(r) for r in result)
                    logging.debug(f"Testcase contribution : {condition}")
                    if not condition:
                        logging.error(
                            f"{behavior_image} behavior found at least one"
                            + " time in the output"
                        )
                    self.res = self.res and condition
        elif k.any():
            if not self.output:
                self.res = False
                logging.error("No output to check the results in")
            else:
                if self.res is None:
                    if vk.unique() or vk.concatenated():
                        self.res = any(all(r) for r in result)
                    elif vk.distributed():
                        self.res = all(any(r) for r in result)
                    logging.debug(f"Testcase contribution : {self.res}")
                    if not self.res:
                        logging.error(
                            f"{behavior_image} behavior not found at least"
                            + "one time in the output"
                        )
                else:
                    condition = False
                    if vk.unique() or vk.concatenated():
                        condition = any(all(r) for r in result)
                    elif vk.distributed():
                        condition = all(any(r) for r in result)
                    logging.debug(f"Testcase contribution : {condition}")
                    if not condition:
                        logging.error(
                            f"{behavior_image} behavior not found at least"
                            + " one time in the output"
                        )
                    self.res = self.res and condition
        elif k.all():
            if not self.output:
                self.res = False
                logging.error("No output to check the results in")
            else:
                if self.res is None:
                    self.res = all(all(r) for r in result)
                    logging.debug(f"Testcase contribution : {self.res}")
                    if not self.res:
                        logging.error(
                            f"{behavior_image} behavior not found"
                            + " in one of the output"
                        )
                else:
                    condition = all(all(r) for r in result)
                    logging.debug(f"Testcase contribution : {condition}")
                    if not condition:
                        logging.error(
                            f"{behavior_image} behavior not found"
                            + " in one of the output"
                        )
                    self.res = self.res and condition
        elif k.empty():
            if not self.output:
                self.res = True
            else:
                self.res = False

        logging.debug(f"Intermediate testcase result : {self.res}")


class ObjProject:
    def __init__(self, file, alt_value):
        self.filename = os.getcwd() + "/" + file
        self.attribute_value = alt_value

    def compile(self, attribute, options):
        if options.SCN_OPTION_USE_ALT_ATTR_VALUE not in options:
            with open(self.filename, "r+") as fp:
                pack, attr = attribute.split(".")
                data = mmap.mmap(fp.fileno(), 0).read().decode()
                regex = (
                    r"^ *(?:package (\w+) is[\w\s]*)?(?:(?!project|package|end))"
                    + rf"(.*(for|\')[ ]*{attr}.* use (.*);)"
                )
                pat = re.compile(regex, flags=re.MULTILINE | re.IGNORECASE)
                results = pat.findall(data)

                for match_p, match_a, x_, value in results:
                    if (match_p == pack) or (match_p == "" and pack == "Project_Level"):
                        self.attribute_value = [
                            r.group(0).replace('"', "")
                            for r in re.finditer('"(.*?)"', value)
                        ]

    def get_attr_value(self):
        return self.attribute_value

    def log(self):
        logging.debug(f"   Attribute -> {self.attribute_value}")


class ObjTestCase:
    def __init__(self, name, file, alt_value, ttype, options):
        self.file = file
        self.ttype = ttype
        self.project = ObjProject(file, alt_value)
        self.res = None
        self.options = Opt(options)
        self.options_list = [flag.name for flag in Opt if flag in self.options]
        self.setup = []
        self.cleanup = []
        if "setup_cmd" in SCN_ATTRIBUTE_TEST_CONFIG[name]:
            for s in SCN_ATTRIBUTE_TEST_CONFIG[name]["setup_cmd"]:
                new_setup_cmd = ObjCmd(s)
                self.setup.append(new_setup_cmd)
        if "cleanup_cmd" in SCN_ATTRIBUTE_TEST_CONFIG[name]:
            for c in SCN_ATTRIBUTE_TEST_CONFIG[name]["cleanup_cmd"]:
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

    def __setup__(self):
        for s in self.setup:
            if type(s.get_cmd()) is list:
                command = list(
                    map(
                        lambda x: x.replace(
                            Pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value, f"-P{self.file}"
                        ),
                        s.get_cmd(),
                    )
                )
                if Opt.SCN_OPTION_USE_CGPR in self.options:
                    if (
                        Tool.SCN_TOOL_GPRBUILD.name in command
                        or Tool.SCN_TOOL_GPRCLEAN.name in command
                    ):
                        command.append(f"--config={Cmd.SCN_CMD_CONFIG_FILE}")
                logging.debug(f"Setting up : {' '.join(command)}")
                bnr.run(command)
            elif s.get_cmd() is not None:
                custom_cmd = s.get_cmd()
                logging.debug(f"Setting up : {custom_cmd}")
                custom_cmd()
            else:
                logging.error(f"Could not launch setup cmd : {s.get_cmd()}")

    def __destroy__(self):
        for c in self.cleanup:
            if type(c.get_cmd()) is list:
                command = list(
                    map(
                        lambda x: x.replace(
                            Pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value, f"-P{self.file}"
                        ),
                        c.get_cmd(),
                    )
                )
                if Opt.SCN_OPTION_USE_CGPR in self.options:
                    if (
                        Tool.SCN_TOOL_GPRBUILD.name in command
                        or Tool.SCN_TOOL_GPRCLEAN.name in command
                    ):
                        command.append(f"--config={Cmd.SCN_CMD_CONFIG_FILE.name}")
                logging.debug(f"Cleaning up : {' '.join(command)}")
                bnr.run(command)
            elif c.get_cmd() is not None:
                custom_cmd = c.get_cmd()
                logging.debug(f"Cleaning up : {custom_cmd}")
                custom_cmd()
            else:
                logging.error(f"Could not launch clean up cmd : {c.get_cmd()}")

    def run(self, config):

        self.__setup__()

        for b in config.behavior_generator():
            if b.cmd() is not None:
                command = list(
                    map(
                        lambda x: x.replace(
                            Pattern.SCN_PRJ_SUBSTITUTE_PATTERN.value, f"-P{self.file}"
                        ),
                        b.cmd(),
                    )
                )
                if Opt.SCN_OPTION_USE_CGPR in self.options:
                    if (
                        Tool.SCN_TOOL_GPRBUILD.tool_name() in command
                        or Tool.SCN_TOOL_GPRCLEAN.tool_name() in command
                    ):
                        command.append(f"--config={Cmd.SCN_CMD_CONFIG_FILE.value}")
                logging.info(f"Launching : \"{' '.join(command)}\"")
                out = bnr.run(command)
                if any(Cmd.SCN_CMD_OUTPUT_FILE.value in s for s in command):
                    logging.debug(
                        "Checking output from " + f"{Cmd.SCN_CMD_OUTPUT_FILE.value}"
                    )
                    new_output = ObjRes(
                        Cmd.SCN_CMD_OUTPUT_FILE.value, b.output(), self.options
                    )
                    self.res = new_output
                else:
                    logging.debug("Checking output directly from bnr.run()")
                    new_output = ObjRes(out.out.split("\n"), b.output(), self.options)
                    self.res = new_output

                self.res.log()
                self.res.compute(
                    b.get_delimiter(),
                    b.get_expected_behavior(self.ttype),
                    config.get_value_kind(),
                    self.project.get_attr_value(),
                    self.options,
                    self.ttype,
                )

            if self.res.get() is None:
                logging.info("No result available, something went wrong !")
            elif self.res.get():
                logging.info("Test OK !")
            else:
                logging.info("Test KO !")

        self.__destroy__()

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
        self.tool = behavior["tool"]
        if "phase" not in behavior:
            self.phase = Phase.SCN_PHASE_NONE
        else:
            self.phase = behavior["phase"]
        self.command = ObjCmd(behavior)
        self.delimiter = behavior["output_delimiter"]
        self.has_behavior = False
        self.scn_case_value = {}
        if "expected_behavior" in behavior:
            self.has_behavior = True
            for b in behavior["expected_behavior"]:
                self.scn_case_value[b] = behavior["expected_behavior"][b]

    def cmd(self):
        return self.command.get_cmd()

    def output(self):
        return self.command.get_cmd_output()

    def get_expected_behavior(self, kind):
        if kind in self.scn_case_value:
            return self.scn_case_value[kind]

    def get_delimiter(self):
        return self.delimiter

    def log(self, i):
        logging.debug(f"[ Behavior - {i} ]")
        logging.debug(f"   Tool      -> {self.tool.name}")
        logging.debug(f"   Phase     -> {self.phase.name}")
        logging.debug(f"   Command   -> {' '.join(self.command.get_cmd())}")
        logging.debug(f"   Output    -> {self.command.get_cmd_output().name}")
        logging.debug(f'   Delimiter -> "{self.delimiter}"')
        if self.has_behavior:
            for b, k in self.scn_case_value.items():
                logging.debug(f"   {b.name} :")
                for kk, kv in k.items():
                    logging.debug(f"      - {kk.name} : {kv}")
        else:
            logging.debug("      - No behavior")


class ObjCmd:
    def __init__(self, command):
        self.cmd = {}
        self.index = None
        self.tool = command["tool"]
        if "phase" not in command:
            tool_phase = Phase.SCN_PHASE_NONE
        else:
            tool_phase = command["phase"]
        self.phase = tool_phase
        self.index = f"{self.tool}.{self.phase}"

        if self.index in SCN_TOOLS_CMD:
            self.cmd[self.index] = SCN_TOOLS_CMD[self.index]

    def compile(self):
        self.cmd[self.index] = SCN_TOOLS_CMD[self.index]

    def get_cmd(self):
        if self.index in self.cmd:
            if "cmd" in self.cmd[self.index]:
                return self.cmd[self.index]["cmd"]
            else:
                logging.error(f"{self.index} has no command defined")
        else:
            logging.error("No command available for this tool and phase")

    def get_cmd_output(self):
        if self.index in self.cmd:
            if self.cmd[self.index]["output"] is not None:
                return self.cmd[self.index]["output"]
            else:
                logging.error(f"{self.index} has no output type defined")
        else:
            logging.error("No command available for this tool and phase")

    def log(self):
        if self.index in self.cmd:
            logging.debug(
                f"   - [ {self.tool.name} - {self.phase.name} ] "
                + f": {self.cmd[self.index]}"
            )
        else:
            logging.error(
                f"   - [ {self.tool.name} - {self.phase.name} ] "
                + "index does not exist"
            )


class ObjConfig:
    def __init__(self, name, options=0):
        self.name = name
        self.value_kind = None
        self.behaviors = []
        self.common_options = Opt(options)
        self.common_options_list = [f.name for f in Opt if f in self.common_options]
        if self.name in SCN_ATTRIBUTE_TEST_CONFIG:
            self.value_kind = SCN_ATTRIBUTE_TEST_CONFIG[self.name]["value_kind"]
            for b in SCN_ATTRIBUTE_TEST_CONFIG[self.name]["test_cmd"]:
                new_behavior = ObjBehavior(b)
                self.behaviors.append(new_behavior)

    def get_name(self):
        return self.name

    def get_index(self):
        p, a = self.name.split(".")
        return f"{p}.{a}"

    def get_common_options(self):
        return self.common_options

    def behavior_generator(self):
        for b in self.behaviors:
            yield b

    def get_value_kind(self):
        return self.value_kind

    def log(self):
        logging.debug(f"[ Config ] Name    -> {self.name} ")
        logging.debug(f"[ Config ] Kind    -> {self.value_kind.name}")
        logging.debug(f"[ Config ] Options -> {self.common_options_list}")
        for i, b in enumerate(self.behaviors, start=1):
            b.log(i)


class ObjScn:
    def __init__(self, attribute, options):
        self.config = None
        self.testcases = []
        self.has_testcase = False
        if "." not in attribute:
            self.config = ObjConfig(f"Project_Level.{attribute}", options)
        else:
            self.config = ObjConfig(attribute, options)
        logging.info(f"Creating a test for {self.config.get_name()} attribute")
        self.config.log()

    def add_testcase(self, file, ttype, alt_value=None, options=0):
        all_options = self.config.get_common_options() + options
        my_testcase = ObjTestCase(
            self.config.get_name(), file, alt_value, ttype, all_options
        )
        my_testcase.compile(self.config.get_name())
        self.testcases.append(my_testcase)
        self.has_testcase = True
        my_testcase.log()

    def run(self):
        logging.info(f"Running {self.config.get_name()} attribute test")
        for i, case in enumerate(self.testcases, start=1):
            logging.info(f"[ Testcase {i} - {case.get_type().name} ]")
            case.run(self.config)

    def has_testcase(self):
        return self.has_testcase
