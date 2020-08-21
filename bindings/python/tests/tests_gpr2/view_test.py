from gpr2.tree import ProjectTree
import json
import os
import pytest


@pytest.mark.data_dir("simple_project")
def test_view():
    with ProjectTree("p.gpr") as tree:
        with tree.root_view as view:
            assert view.name == "P"
            assert os.path.basename(view.path_name) == "p.gpr"
            assert view.id is not None
            assert len(view.sources()) == 2, json.dumps(view.sources(), indent=2)
        assert view.id is None, "view not deallocated"
