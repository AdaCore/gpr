from gpr2.tree import ProjectTree, Options
import json
import os
import pytest


@pytest.mark.data_dir("simple_project")
def test_view():
    with ProjectTree(Options("p.gpr")) as tree:
        tree.update_sources()
        view = tree.root_project
        assert view.name == "P"
        assert os.path.basename(view.path_name) == "p.gpr"
        assert view.id is not None
        assert len(view.sources) == 1, json.dumps(view.sources, indent=2)
        assert len(view.visible_sources) == 1, json.dumps(view.visible_sources, indent=2)


@pytest.mark.data_dir("attribute_project")
def test_attributes():
    with ProjectTree(Options("attribute_project.gpr")) as tree:
        tree.update_sources()
        view = tree.root_project
        print(view.sources)
        assert len(view.sources) == 4
        # Attributes are not implemented
        # assert view.attribute(
        #     name="switches", pkg="compiler", filename="b.ads"
        # ).value == ["-fb"]
        # assert view.attribute(
        #     name="switches", pkg="compiler", filename="c.ads"
        # ).value == ["-fothers"]

        # assert view.attribute(
        #     name="switches", pkg="compiler", language="Ada"
        # ).value == ["-fada"]
        # assert view.attribute(
        #     name="switches", pkg="compiler", language="ada"
        # ).value == ["-fada"]
        # assert view.attribute(
        #     name="switches", pkg="compiler", filename="ada_pkg.ads"
        # ).value == ["-fada_pkg"]
        # assert view.attribute(
        #     name="switches", pkg="compiler", filename="./ada_pkg.ads"
        # ).value == ["-fada_pkg"]
        # assert view.attribute(
        #     name="switches", pkg="compiler", filename="ada_pkg.adb"
        # ).value == ["-fada"]
