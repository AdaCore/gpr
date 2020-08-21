from e3.env import Env
from e3.fs import rm, sync_tree
from e3.os.fs import cd
import pytest
import os
import tempfile
import logging

ROOT_DIR = os.path.dirname(os.path.abspath(__file__))


def pytest_configure(config):
    config.addinivalue_line("markers", "data_dir(dir) :directory containing test data")


@pytest.fixture(autouse=True)
def env_protect(request):
    """Protection against environment change.

    The fixture is enabled for all tests and does the following:

    * store/restore env between each tests
    * create a temporary directory and do a cd to it before each
      test. The directory is automatically removed when test ends
    """
    Env().store()
    tempd = tempfile.mkdtemp()
    cd(tempd)

    project_marker = request.node.get_closest_marker("data_dir")
    if project_marker is not None:
        project_dir = os.path.join(ROOT_DIR, "projects", project_marker.args[0])
        logging.debug(f"use project dir {project_dir}")
        sync_tree(project_dir, tempd)

    def restore_env():
        Env().restore()
        rm(tempd, True)

    request.addfinalizer(restore_env)
