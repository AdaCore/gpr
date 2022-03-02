from gpr2.tree import ProjectTree
import json
import os
import pytest


@pytest.mark.data_dir("simple_project")
def test_view():
    with ProjectTree("p.gpr") as tree:
        view = tree.root_view
        assert view.name == "P"
        assert os.path.basename(view.path) == "p.gpr"
        assert view.id is not None
        assert len(view.sources()) == 1, json.dumps(view.sources(), indent=2)


@pytest.mark.data_dir("attribute_project")
def test_attributes():
    with ProjectTree("attribute_project.gpr") as tree:
        view = tree.root_view
        print(view.sources())
        assert len(view.sources()) == 4
        assert view.attribute(
            name="switches", pkg="compiler", filename="b.ads"
        ).value == ["-fb"]
        assert view.attribute(
            name="switches", pkg="compiler", filename="c.ads"
        ).value == ["-fothers"]

        assert view.attribute(
            name="switches", pkg="compiler", language="Ada"
        ).value == ["-fada"]
        assert view.attribute(
            name="switches", pkg="compiler", language="ada"
        ).value == ["-fada"]
        assert view.attribute(
            name="switches", pkg="compiler", filename="ada_pkg.ads"
        ).value == ["-fada_pkg"]
        assert view.attribute(
            name="switches", pkg="compiler", filename="./ada_pkg.ads"
        ).value == ["-fada_pkg"]
        assert view.attribute(
            name="switches", pkg="compiler", filename="ada_pkg.adb"
        ).value == ["-fada"]
