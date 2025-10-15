#
#  Copyright (C) 2020-2025, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from gpr2.capi import LibGPR2
from gpr2.view import ProjectView
from gpr2.message import Message
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from types import TracebackType
    from typing import Dict, Optional, Type, List


class Options:

    def __init__(self, project_file=None, context=None, target=None):
        self._options = {}
        self._options["P"] = project_file
        self._options["context"] = context
        self._options["target"] = target


class ProjectTree:
    """Represent a tree of project files."""

    def __init__(
        self,
        options: Options,
        with_runtime: bool = False,
    ) -> None:
        """Load a project tree."""

        self._id = ""
        self._project_data = LibGPR2.tree_load(
            request={
                "options": options._options,
                "with_runtime": with_runtime,
            }
        )

        self._id = self._project_data["tree_id"]
        self.is_loaded = self._project_data["is_loaded"]

    def __del__(self):
        LibGPR2.tree_destructor(request={"tree_id": self._id})

    def __enter__(self) -> ProjectTree:
        return self

    def __exit__(
        self,
        _type: Optional[Type[BaseException]],
        _val: Optional[BaseException],
        _tb: Optional[TracebackType],
    ) -> None:
        pass

    @property
    def artifacts_directory(self) -> str | None:
        """
        Tries to return a directory that can be used to store artifacts that
        are global to the tree.
        This returns the object directory of the root view if available, else
        it returns the root view's project directory.
        **Important note** project directories may not be writable, as only
        object dirs are required to have read/write access. So this function
        needs to be used with care.
        """
        answer = LibGPR2.tree_artifacts_directory(request={"tree_id": self._id})
        return answer["artifacts_directory"]

    @property
    def context(self):
        """Returns the Context for the given project tree"""
        answer = LibGPR2.tree_context(request={"tree_id": self._id})
        return answer["context"]

    @property
    def log_messages(self):
        """Returns the Logs, this contains information, warning and error
        messages found while handling the project.
        """
        answer = LibGPR2.tree_log_messages(request={"tree_id": self._id})
        return [Message.from_dict(msg) for msg in answer["messages"]]

    def set_context(self, context):
        """
        Sets the context for the project tree. The callback Changed is called
        for any project view which is impacted by this change of context, i.e.
        if the project view references directly or indirectly an external
        variable.

        Returns False if the new context generate an error while re-loading the
        project tree.
        """
        answer = LibGPR2.tree_set_context(
            request={"tree_id": self._id, "context": context}
        )
        return answer["success"]

    @property
    def target(self):
        """Returns the target for the project tree"""
        answer = LibGPR2.tree_target(request={"tree_id": self._id})
        return answer["target"]

    def update_sources(self) -> None:
        """
        Ensures that all views' sources are up-to-date.
        Option selects the information that will be gathered on the sources. The
        more information is requested, the slower is the update operation.
        No_Error: when set prevents the update of sources to generate errors
        The errors are then replaced by warnings.

        Used by the Load function when its Artifacts_Info_Level is set
        to fetch sources.
        """
        # Invalidate all sources before recomputing (GPR2 bug?)
        answer = LibGPR2.tree_update_sources(request={"tree_id": self._id})

    @property
    def root_project(self) -> ProjectView:
        """Return the root project view of the tree.

        :return: the root project view
        """
        answer = LibGPR2.tree_root_project(request={"tree_id": self._id})
        return ProjectView(tree=self, id=answer["view_id"])

    @property
    def runtime_project(self) -> ProjectView | None:
        """Returns the runtime project for the given tree"""
        answer = LibGPR2.tree_runtime_project(request={"tree_id": self._id})
        view_id = answer["view_id"]
        return None if view_id is None else ProjectView(tree=self, id=view_id)
