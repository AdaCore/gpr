from gpr2.tree import ProjectTree
from gpr2.view import ProjectView
from gpr2 import GPR2Error
import pytest
import logging


def test_load_unexisting_project():
    """Load a non existing project.

    A GPR2Error exception should be raised with a non-existing project
    message.
    """
    with pytest.raises(GPR2Error) as exc_info:
        ProjectTree("non_existing.gpr")
    exc_info.match("non_existing.gpr: no such file")


@pytest.mark.data_dir("simple_project")
def test_load_simple_project():
    """Load a simple project."""
    tree = ProjectTree("p.gpr")
    tree.close()

    with ProjectTree("p.gpr") as tree:
        assert tree.id is not None

        assert tree.language_properties("ada").dependency_ext == ".ali"
    assert tree.id is None


@pytest.mark.data_dir("simple_project")
def test_root_view():
    """Get the root view of a given project tree."""
    tree = ProjectTree("p.gpr")
    view = tree.root_view
    assert isinstance(view, ProjectView)


@pytest.mark.data_dir("invalid_project")
def test_load_non_parsable_project():
    """Load a project with syntax errors."""
    with pytest.raises(GPR2Error) as exc_info:
        ProjectTree("invalid.gpr")
    exc_info.match("syntax error")


@pytest.mark.data_dir("context_project")
def test_context():
    """Load a project and retrieve the context."""
    with ProjectTree("p.gpr") as tree:
        assert len(tree.context) == 0, tree.context

    with ProjectTree("p.gpr", context={"VAR1": "valnico", "VAR2": ""}) as tree:
        assert len(tree.context) == 2, tree.context

    with ProjectTree("p.gpr", context={"VAR1": "valnico", "VAR3": ""}) as tree:
        assert len(tree.context) == 2, tree.context


@pytest.mark.data_dir("simple_project")
def test_messages():
    """Fetch project messages."""
    with ProjectTree("p.gpr") as tree:
        tree.fetch_messages()
        logging.info("\n".join([str(msg) for msg in tree.messages]))
        assert len(tree.messages) > 0
        former_length = len(tree.messages)
        tree.fetch_messages()
        assert len(tree.messages) == former_length


@pytest.mark.data_dir("external_target_project")
def test_target():
    """Target test."""
    logging.info("Target set by external default value")
    with ProjectTree("p.gpr") as tree:
        assert tree.target == "dummy", "target not set"

    logging.info("Target set using target parameter should bypass external")
    with ProjectTree("p.gpr", target="x86_64-pc-linux-gnu") as tree:
        assert tree.target == "x86_64-pc-linux-gnu"

    with ProjectTree("p.gpr", target="dummy-target") as tree:
        assert tree.target == "dummy-target"

    logging.info("Target parameter should be used in priority")
    with ProjectTree(
        "p.gpr", target="real-target", context={"TARGET_NAME": "new-target"}
    ) as tree:
        assert tree.target == "real-target"


@pytest.mark.xfail
@pytest.mark.data_dir("external_target_project")
def test_target2():
    """Target test."""
    logging.info("Target set using context.")
    with ProjectTree("p.gpr", context={"TARGET_NAME": "new-target"}) as tree:
        assert tree.target == "new-target"

        logging.info("Check that context change implies change in target")
        tree.context = {"TARGET_NAME": "new-target2"}
        assert tree.target == "new-target2"
        tree.context = {}
        assert tree.target == "dummy"
