from gpr2.capi import LibGPR2
from gpr2.tree import ProjectTree
from gpr2 import GPR2Error
from ctypes import c_char_p, byref
import json
import pytest


def test_load_gpr2():
    """Ensure that C API can be loaded."""
    LibGPR2.load_api()


def test_invalid_json_request():
    """Invalid request paramater in C binding."""
    fun = LibGPR2.gpr2_request()
    answer = c_char_p()
    status = fun(1, '[""],'.encode("utf-8"), byref(answer))
    answer = json.loads(answer.value.decode("utf-8"))
    assert status == 1


@pytest.mark.data_dir("simple_project")
def test_missing_json_member_request():
    """Missing manadatory key in C binding request."""
    with ProjectTree("p.gpr") as tree:
        with pytest.raises(GPR2Error) as exc_info:
            LibGPR2.view_attribute({"tree_id": tree.root_view.tree.id,
                                    "view_id": tree.root_view.id})
    exc_info.match("expected string type")
