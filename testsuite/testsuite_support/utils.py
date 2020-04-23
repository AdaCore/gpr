"""
Helper for Python scripts in testcases.
"""

import os
import re
from e3.env import Env


def filter(output, actual, expected):
    """Replace in output all actual occurrences by expected."""
    return output.replace(actual, expected)


def filter_cwd(output):
    """
    Remove all occurrences of the current working directory from in output.
    """
    if Env().host.platform.endswith('windows'):
        #  On windows, path are case insensitive
        return re.compile(re.escape(os.getcwd()),
                          re.IGNORECASE).sub('', output)
    else:
        return output.replace(os.getcwd(), '')
