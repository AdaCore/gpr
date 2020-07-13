from gpr2.tree import ProjectTree
import os


def test_view_infos():
    with ProjectTree("p.gpr") as tree:
        view = tree.root_view
        assert view.name == "P"
        assert os.path.basename(view.path_name) == "p.gpr"
