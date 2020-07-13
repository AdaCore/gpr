from e3.env import Env
from e3.fs import rm, sync_tree
from e3.os.fs import cd
import pytest
import os
import tempfile


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

    data_dir = os.path.join(
        os.path.dirname(request.fspath.strpath), request.function.__name__
    )
    print(data_dir)
    if os.path.isdir(data_dir):
        sync_tree(data_dir, tempd)

    def restore_env():
        Env().restore()
        rm(tempd, True)

    request.addfinalizer(restore_env)
