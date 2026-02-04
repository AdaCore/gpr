from gpr2.tree import ProjectTree, Options
from gpr2.view import ProjectView
from gpr2 import GPR2Error
from e3.fs import mkdir
from e3.sys import interpreter
from e3.os.process import Run
import os
import pytest
import logging


def test_load_unexisting_project():
    """Load a non existing project.

    A GPR2Error exception should be raised with a non-existing project
    message.
    """
    with ProjectTree(Options("non_existing.gpr")) as tree:
        assert not tree.is_loaded
        assert len(tree.log_messages) != 0

@pytest.mark.data_dir("simple_project")
def test_load_simple_project():
    """Load a simple project."""
    tree = ProjectTree(Options("p.gpr"))
    assert tree.is_loaded

    with ProjectTree(Options("p.gpr")) as tree:
        assert tree._id is not None
    assert tree._id is not None


@pytest.mark.data_dir("simple_project")
def test_root_view():
    """Get the root view of a given project tree."""
    tree = ProjectTree(Options("p.gpr"))
    view = tree.root_project
    assert isinstance(view, ProjectView)


@pytest.mark.data_dir("invalid_project")
def test_load_non_parsable_project():
    """Load a project with syntax errors."""

    with ProjectTree(Options("invalid.gpr")) as tree:
        print(tree.log_messages)
        assert not tree.is_loaded
        assert len(tree.log_messages) != 0


@pytest.mark.data_dir("context_project")
def test_context():
    """Load a project and retrieve the context."""
    with ProjectTree(Options("p.gpr")) as tree:
        assert len(tree.context) == 0, tree.context

    with ProjectTree(Options("p.gpr", context={"VAR1": "valnico", "VAR2": ""})) as tree:
        assert len(tree.context) == 2, tree.context

    with ProjectTree(Options("p.gpr", context={"VAR1": "valnico", "VAR3": ""})) as tree:
        assert len(tree.context) == 2, tree.context


@pytest.mark.data_dir("simple_project")
def test_messages():
    """Fetch project messages."""
    with ProjectTree(Options("p.gpr")) as tree:
        logging.info("\n".join([str(msg) for msg in tree.log_messages]))
        assert len(tree.log_messages) > 0


@pytest.mark.data_dir("external_target_project")
def test_target():
    """Target test."""
    logging.info("Target set by external default value")
    with ProjectTree(Options("p.gpr")) as tree:
        assert tree.target == "dummy", "target not set"

    logging.info("Target set using target parameter should bypass external")
    with ProjectTree(Options("p.gpr", target="x86_64-pc-linux-gnu")) as tree:
        assert tree.target == "x86_64-pc-linux-gnu"

    with ProjectTree(Options("p.gpr", target="dummy-target")) as tree:
        assert tree.target == "dummy-target"

    logging.info("Target parameter should be used in priority")
    with ProjectTree(
        Options("p.gpr", target="real-target", context={"TARGET_NAME": "new-target"})
    ) as tree:
        assert tree.target == "real-target"


@pytest.mark.xfail
@pytest.mark.data_dir("external_target_project")
def test_target2():
    """Target test."""
    logging.info("Target set using context.")
    with ProjectTree(Options("p.gpr", context={"TARGET_NAME": "new-target"})) as tree:
        assert tree.target == "new-target"

        logging.info("Check that context change implies change in target")
        tree.context = {"TARGET_NAME": "new-target2"}
        assert tree.target == "new-target2"
        tree.context = {}
        assert tree.target == "dummy"


@pytest.mark.xfail # `project_dir` is not supported
@pytest.mark.data_dir("simple_project")
def test_project_dir():
    mkdir("working_dir")
    with ProjectTree(project_dir="working_dir") as tree:
        assert tree.project_dir == os.path.dirname(tree.root_view.path)
        assert tree.project_dir == tree.root_view.dir
        assert tree.project_dir == os.path.abspath("working_dir")


@pytest.mark.data_dir("simple_project")
def test_source_list():
    with ProjectTree(Options("p.gpr")) as tree:
        tree.update_sources()
        source_list = tree.root_project.sources
        assert len(source_list) == 1
        assert os.path.basename(source_list[0].path_name) == "main.adb"

        # adding a source in the current directory
        with open("ada_pkg.ads", "w") as fd:
            fd.write("package ada_pkg is null; end ada_pkg;")

        # Project sources should not update automatically
        source_list = tree.root_project.sources
        assert len(source_list) == 1

        # A call to invalidate will cause an update on next
        # call to sources
        tree.update_sources()
        source_list = tree.root_project.sources
        assert len(source_list) == 2

        with open("ada_pkg2.ads", "w") as fd:
            fd.write("package ada_pkg2 is null; end ada_pkg2;")

        # Call to update_source_list should update the list of
        # sources
        tree.update_sources()
        source_list = tree.root_project.sources
        assert len(source_list) == 3

    with ProjectTree(Options("p.gpr")) as tree:
        tree.update_sources()
        source_list = tree.root_project.sources
        assert len(source_list) == 3


@pytest.mark.xfail # `cli_load` is not supported
@pytest.mark.data_dir("simple_project")
def test_cli():
    p = Run(
        [
            interpreter(),
            "-c",
            "\n".join(
                [
                    "from gpr2.tree import ProjectTree, Options",
                    "import argparse",
                    "from gpr2.main import add_project_options",
                    "arg_parser = argparse.ArgumentParser()",
                    "add_project_options(arg_parser)",
                    "args = arg_parser.parse_args()",
                    "ProjectTree.cli_load(args)",
                ]
            ),
            "-P",
            "p.gpr",
        ]
    )
    assert p.status == 0, p.out

@pytest.mark.data_dir("simple_project")
def test_default_iterate_simple():
    with ProjectTree(Options("p.gpr")) as tree:
        for view in tree:
            assert view.name == 'P'

@pytest.mark.data_dir("extending_project")
def test_default_iterate_extending():
    with ProjectTree(Options("e.gpr")) as tree:
        names = set()
        for view in tree:
            assert view.name not in names, f"view {view.name} already seen"
            names.add(view.name)
        assert len(names) == 5, f"expected 5 views, got {len(names)}"
        assert 'A' in names, "view A not found"
        assert 'B' in names, "view B not found"
        assert 'C' in names, "view C not found"
        assert 'D' in names, "view D not found"
        assert 'E' in names, "view E not found"

@pytest.mark.data_dir("simple_project")
def test_ada_source_closure():
    with ProjectTree(Options("p.gpr")) as tree:
        tree.update_sources();
        closure = tree.ada_closure()
        assert len(closure) == 1
        assert os.path.basename (closure[0].path_name) == 'main.adb'
