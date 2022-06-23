from testsuite_support.driver.driver import ObjScn
from testsuite_support.driver.driver_constants import ObjScnCaseValue
from testsuite_support.driver.driver_constants import SCN_TOOLS_CMD
from testsuite_support.driver.driver_constants import ObjScnPhase as osp
from testsuite_support.driver.driver_constants import ObjScnTool as ost
import logging

my_scn = None


def edit_custom_command(custom_cmd=None, slot=osp.SCN_PHASE_CUSTOM_SLOT_1) -> None:
    """ This function allows the user to set a custom command to be executed
        whe setting up a scenario.
        For instance, if the user want to modify a value in a specific file between
        two calls of GPRtools.
        This function will replace the SCN_TOOL_CUSTOM value in the scenario
        description in driver_db.py
    """
    logging.debug(f"Edit [ {ost.SCN_TOOL_CUSTOM} - {slot} ]")
    logging.debug(f'From : {SCN_TOOLS_CMD[f"{ost.SCN_TOOL_CUSTOM}.{slot}"]} '
                  + f'/ To : {custom_cmd}')
    SCN_TOOLS_CMD[f"{ost.SCN_TOOL_CUSTOM}.{slot}"] = custom_cmd


def create_scenario(attribute: str, log_level: int = logging.INFO):
    """ This function create a scenario object
    """
    logging.basicConfig(level=log_level, format="[%(levelname)s] - %(message)s")
    global my_scn
    if my_scn:
        logging.error("A scenario already exists. It will be overwritten")
    my_scn = ObjScn(attribute)


def add_testcase(*, file, type, altvalue=None, options=0):
    """ This function adds a testcase to an existing scenario object.
    """
    if not isinstance(type, ObjScnCaseValue):
        logging.error("type must be an instance of ObjScnCaseValue Enum")
    global my_scn
    if not my_scn:
        logging.error("No existing scenario to add a testcase to."
                      + " Please create a scenario")
        return
    my_scn.add_testcase(file, type, altvalue, options)


def run():
    """ Run the previously set up scenario.
        The object is automatically destroyed after the execution.
    """
    global my_scn
    if not my_scn:
        logging.error("No existing scenario to run. Please create a scenario")
        return
    if not my_scn.has_testcase:
        logging.error("No testcase to run. Please add a testcase")
        return
    my_scn.run()
    my_scn = None
