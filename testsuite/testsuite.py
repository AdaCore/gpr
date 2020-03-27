#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the gpr2 testsuite.
"""

import os
from gnatpython.env import Env

from testsuite_support import Testsuite


if __name__ == '__main__':
    Env().add_search_path('PYTHONPATH',
                          os.path.join(os.getcwd(), 'testsuite_support'))
    Testsuite(os.path.dirname(__file__)).testsuite_main()
