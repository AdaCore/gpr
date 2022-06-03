from gpr2.tree import ProjectTree
from e3.fs import rm
import os
import pytest


@pytest.mark.data_dir("simple_project_with_ali")
def test_source_dependencies():
    with ProjectTree("p.gpr") as tree:
        view = tree.root_view
        sources = view.sources()
        assert len(sources) == 5

        main = next(
            source for source in sources if os.path.basename(source.path) == "main.adb"
        )

        # We have some .ali files so we expect to get information from them
        assert len(main.dependencies) == 4
        assert {os.path.basename(s.path) for s in main.dependencies} == {
            "ada_pkg1.ads",
            "ada_pkg2.ads",
            "ada_pkg3.ads",
            "main.adb",
        }
        # Remove the .ali files
        rm("*.ali")

        # Update the dependencies
        main.update_dependencies(allow_source_parsing=False)

        # Why do we still have them with previous value ?
        assert len(main.dependencies) == 4
        assert {os.path.basename(s.path) for s in main.dependencies} == {
            "ada_pkg1.ads",
            "ada_pkg2.ads",
            "ada_pkg3.ads",
            "main.adb",
        }

        # Force full source invalidation
        tree.invalidate_source_list()
        tree.update_source_list()

        # Recompute the deps for main.adb
        sources = view.sources()
        main = next(
            source for source in sources if os.path.basename(source.path) == "main.adb"
        )

        # Seems that source parsing is enabled even when not selected. Result is very
        # distinct.

        # If there is no ALI file we should not have any dep information
        assert len(main.dependencies) == 0

        # Update source_infos using source parsing
        main.update_dependencies(allow_source_parsing=True)
        assert {os.path.basename(s.path) for s in main.dependencies} == {
            "ada_pkg1.ads",
            "ada_pkg2.ads",
            "main.adb",
        }
