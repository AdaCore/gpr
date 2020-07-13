from __future__ import annotations
from gpr2 import GPR2Error
from gpr2.capi import LibGPR2
from gpr2.view import ProjectView
from typing import TYPE_CHECKING
import os


if TYPE_CHECKING:
    from types import TracebackType
    from typing import Dict, Optional, Type


class ProjectTree:
    """Represent a tree of project files."""

    def __init__(
        self, project_file: str, context: Optional[Dict[str, str]] = None
    ) -> None:
        """Load a project tree.

        :param project_file: the root project file to load
        :param context: the external variables values. The key is the variable
            name. If None no external variables are passed.
        """
        # Project existence should be handled by the GPR2 load function. But
        # currently the API returns a syntax error. Remove check when this is
        # fixed.
        if not os.path.isfile(project_file):
            raise GPR2Error(f"{project_file}: no such file")

        request = {"filename": project_file, "context": context if context else {}}
        answer = LibGPR2.gpr2_prj_tree_load_autoconf(request)
        self.id = answer["tree_id"]

    @property
    def root_view(self) -> ProjectView:
        """Return the root project view of the tree.

        :return: the root project view
        """
        answer = LibGPR2.gpr2_prj_tree_root_project({"tree_id": self.id})
        return ProjectView(id=answer["view_id"])

    def close(self) -> None:
        """Unload a project tree."""
        LibGPR2.gpr2_prj_tree_unload({"tree_id": self.id})
        self.id = None

    def __enter__(self) -> ProjectTree:
        return self

    def __exit__(
        self,
        _type: Optional[Type[BaseException]],
        _val: Optional[BaseException],
        _tb: Optional[TracebackType],
    ) -> None:
        self.close()
