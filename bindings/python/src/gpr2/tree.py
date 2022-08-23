#
#  Copyright (C) 2020-2022, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from gpr2 import GPR2Error
from gpr2.capi import LibGPR2
from gpr2.view import ProjectView
from gpr2.message import Message
from typing import TYPE_CHECKING
import os


if TYPE_CHECKING:
    from types import TracebackType
    from typing import Dict, Optional, Type, List


class ProjectTree:
    """Represent a tree of project files."""

    def __init__(
        self,
        project: Optional[str] = None,
        context: Optional[Dict[str, str]] = None,
        config: Optional[str] = None,
        build_path: Optional[str] = None,
        subdirs: Optional[str] = None,
        src_subdirs: Optional[str] = None,
        check_shared_lib: bool = True,
        absent_dir_error: bool = False,
        implicit_with: Optional[List[str]] = None,
        target: Optional[str] = None,
        language_runtimes: Optional[Dict[str, str]] = None,
        project_dir: Optional[str] = None,
    ) -> None:
        """Load a project tree.

        :param project: the root project file to load
        :param context: the external variables values. The key is the variable
            name. If None no external variables are passed.
        :param config: path to a GPR configuration project
        :param build_path: object dir for the tree. This parameter can be used for
            out-of-tree builds.
        :param subdirs:
        """
        # Project existence should be handled by the GPR2 load function. But
        # currently the API returns a syntax error. Remove check when this is
        # fixed.
        if project is not None and not os.path.isfile(project):
            raise GPR2Error(f"{project}: no such file")

        self.project = project
        self._context = context if context is not None else {}
        self.config = config
        self.build_path = build_path
        self.subdirs = subdirs
        self.src_subdirs = src_subdirs
        self.check_shared_lib = check_shared_lib
        self.absent_dir_error = absent_dir_error
        self.implicit_with = implicit_with
        self.target = target
        self.language_runtimes = language_runtimes

        if project_dir is not None:
            self.project_dir = os.path.abspath(project_dir)
        else:
            self.project_dir = os.getcwd()

        self._project_data = LibGPR2.tree_load(
            request={
                "filename": project,
                "context": self._context,
                "build_path": self.build_path,
                "subdirs": self.subdirs,
                "src_subdirs": self.src_subdirs,
                "project_dir": self.project_dir,
                "check_shared_lib": self.check_shared_lib,
                "absent_dir_error": self.absent_dir_error,
                "implicit_with": self.implicit_with,
                "config": self.config,
                "target": self.target,
                "runtimes": self.language_runtimes,
            }
        )

        self.id = self._project_data["id"]

        self.messages = []
        self.properties_for_languages = {}
        self.target = self._project_data["target"]
        self.search_paths = self._project_data["search_paths"]
        self._context = self._project_data["context"]

        # If False it means the source list has not been computed or has been
        # invalidated. This attribute is used in gpr2.view module to control
        # how source list is loaded whenever we ask for sources from a view.
        # This is mainly used to avoid triggering parsing of all the sources
        # on call to ProjectView.sources. This should be eliminated in case
        # GPR2 behaviour changes.
        self.have_source_list = False

    def invalidate_source_list(self) -> None:
        """Invalidate current list of sources."""
        LibGPR2.tree_invalidate_source_list({"tree_id": self.id})
        self.have_source_list = False

    def update_source_list(self) -> None:
        """Populate/Update the list of sources associated with the tree."""
        # Invalidate all sources before recomputing (GPR2 bug?)
        self.invalidate_source_list()
        LibGPR2.tree_update_source_list({"tree_id": self.id})
        self.have_source_list = True

    @property
    def views(self) -> list[ProjectView]:
        """Return the list of views sorted in topological order."""
        return [ProjectView(tree=self, id=id) for id in self._project_data["views"]]

    @property
    def context(self):
        """Return the context of the tree.

        :return: the context
        """
        return self._context

    @context.setter
    def context(self, value: Dict[str, str]):
        LibGPR2.tree_set_context({"tree_id": self.id, "context": value})
        self._context = value

    @property
    def root_view(self) -> ProjectView:
        """Return the root project view of the tree.

        :return: the root project view
        """
        return ProjectView(tree=self, id=self._project_data["root_view"])

    def fetch_messages(self) -> List[Message]:
        """Fetch new messages.

        Note that unread messages are also added to self.messages

        :return: unread messages
        """
        answer = LibGPR2.tree_log_messages(
            {
                "tree_id": self.id,
                "information": True,
                "warning": True,
                "error": True,
                "read": False,
                "unread": True,
            }
        )
        messages = [Message.from_dict(msg) for msg in answer["messages"]]
        self.messages += messages
        return messages

    def close(self) -> None:
        """Unload a project tree."""
        LibGPR2.tree_unload({"tree_id": self.id})
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

    @classmethod
    def cli_load(cls, args) -> ProjectTree:
        """Load a project using arguments passed on the CLI."""
        if args.context is not None:
            context = dict(k.split("=", 1) for k in args.context)
        else:
            context = None
        return cls(
            project=args.project,
            context=context,
            config=args.config,
            subdirs=args.subdirs,
            src_subdirs=args.src_subdirs,
            implicit_with=args.implicit_with,
            target=args.target,
        )
