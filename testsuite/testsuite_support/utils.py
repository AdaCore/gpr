"""
Helper for Python scripts in testcases.
"""

import os


def filter(output, actual, expected):
    """Replace in output all actual occurrences by expected."""
    return output.replace(actual, expected)


def filter_cwd(output):
    """
    Remove all occurrences of the current working directory from in output.
    """
    return output.replace(os.getcwd(), '')
