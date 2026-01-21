#
#  Copyright (C) 2020-2025, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from ctypes import CDLL, c_char_p, c_int, POINTER, byref
from functools import partial
from gpr2 import GPR2Error
import json
import os
import sys


class LibGPR2:

    METHOD_LIST = {
        "tree_artifacts_directory": 1,
        "tree_context": 2,
        "tree_destructor": 3,
        "tree_load": 4,
        "tree_log_messages": 5,
        "tree_root_project": 6,
        "tree_runtime_project": 7,
        "tree_set_context": 8,
        "tree_target": 9,
        "tree_update_sources": 10,
        "view_destructor": 11,
        "view_executables": 12,
        "view_object_directory": 13,
        "view_sources": 14,
    }

    # ??? not portable ???
    LIBNAME = "libgpr2c.dll" if sys.platform == "win32" else "libgpr2c.so"
    LIB = None

    @classmethod
    def lib(cls):
        if cls.LIB is None:
            lib_file = os.path.join(
                os.path.dirname(os.path.abspath(__file__)), "lib", cls.LIBNAME
            )
            if not os.path.isfile(lib_file):
                lib_file = cls.LIBNAME
            cls.LIB = CDLL(lib_file)
        return cls.LIB

    @classmethod
    def gpr2_request(cls):
        result = cls.lib().gpr2_request
        result.argtypes = [c_int, c_char_p, POINTER(c_char_p)]
        result.restype = c_int
        return result

    @classmethod
    def load_api(cls) -> None:

        for method_name, method_id in cls.METHOD_LIST.items():
            c_fun = cls.gpr2_request()

            # Create a function that will serialize request and unserialize answers
            # The function also ensure a Python exception is raised whenever an
            # exception occurs.
            def fun(c_fun, method_id, request):
                answer = c_char_p()
                status = c_fun(
                    method_id, json.dumps(request).encode("utf-8"), byref(answer)
                )
                result = json.loads(answer.value.decode("utf-8"))
                if status != 0:
                    raise GPR2Error(
                        f"error:{result['error_name']}:{result['error_msg']}"
                    )
                return result["result"]

            setattr(cls, method_name, partial(fun, c_fun, method_id))


LibGPR2.load_api()
