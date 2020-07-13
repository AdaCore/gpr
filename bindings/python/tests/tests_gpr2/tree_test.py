from gpr2.tree import ProjectTree
from gpr2.view import ProjectView
from gpr2 import GPR2Error
import pytest


def test_load_unexisting_project():
    """Load a non existing project.

    A GPR2Error exception should be raised with a non-existing project
    message.
    """
    with pytest.raises(GPR2Error) as exc_info:
        ProjectTree("non_existing.gpr")
    exc_info.match("non_existing.gpr: no such file")


def test_load_simple_project():
    """Load a simple project."""
    tree = ProjectTree("p.gpr")
    tree.close()

    with ProjectTree("p.gpr") as tree:
        assert tree.id is not None
    assert tree.id is None


def test_root_view():
    """Get the root view of a given project tree."""
    tree = ProjectTree("p.gpr")
    view = tree.root_view
    assert isinstance(view, ProjectView)


def test_load_non_parsable_project():
    """Load a project with syntax errors."""
    with pytest.raises(GPR2Error) as exc_info:
        ProjectTree("invalid.gpr")
    exc_info.match("syntax error")
