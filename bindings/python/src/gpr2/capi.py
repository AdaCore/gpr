from __future__ import annotations
from ctypes import CDLL, c_char_p, c_int, POINTER, byref
from functools import partial
from gpr2 import GPR2Error
import json
import os


class LibGPR2:

    METHOD_LIST = [
        "gpr2_prj_tree_context",
        "gpr2_prj_tree_language_properties",
        "gpr2_prj_tree_load_autoconf",
        "gpr2_prj_tree_log_messages",
        "gpr2_prj_tree_properties",
        "gpr2_prj_tree_root_project",
        "gpr2_prj_tree_unload",
        "gpr2_prj_tree_get_view",
        "gpr2_prj_view_attribute",
        "gpr2_prj_view_information"
    ]

    # ??? not portable ???
    LIBNAME = "libgpr2c.so"
    LIB = None

    @classmethod
    def lib(cls):
        if cls.LIB is None:
            lib_file = os.path.join(
                os.path.dirname(os.path.abspath(__file__)), "libgpr2c.so"
            )
            if not os.path.isfile(lib_file):
                lib_file = "libgpr2c.so"
            cls.LIB = CDLL(lib_file)
        return cls.LIB

    @classmethod
    def cfun(cls, name):
        result = getattr(cls.lib(), name)
        result.argtypes = [c_char_p, POINTER(c_char_p)]
        result.restype = c_int
        return result

    @classmethod
    def load_api(cls) -> None:

        for method in cls.METHOD_LIST:
            c_fun = cls.cfun(method)

            # Create a function that will serialize request and unserialize answers
            # The function also ensure a Python exception is raised whenever an
            # exception occurs.
            def fun(c_fun, request):
                answer = c_char_p()
                status = c_fun(json.dumps(request).encode("utf-8"), byref(answer))
                result = json.loads(answer.value.decode("utf-8"))
                if status != 0:
                    raise GPR2Error(
                        f"error:{result['error_name']}:{result['error_msg']}"
                    )
                return result["result"]

            setattr(cls, method, partial(fun, c_fun))


LibGPR2.load_api()
