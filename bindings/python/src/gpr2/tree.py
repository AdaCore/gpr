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
    from gpr2.config import ProjectConfig


class LanguageProperties:
    def __init__(
        self,
        language: str,
        runtime: Optional[str],
        object_ext: str,
        dependency_ext: str,
    ):
        self.language = language
        self.runtime = runtime
        self.object_ext = object_ext
        self.dependency_ext = dependency_ext

    @classmethod
    def from_dict(cls, language, data):
        return LanguageProperties(
            language=language,
            runtime=data.get("runtime"),
            object_ext=data["object_suffix"],
            dependency_ext=data["dependency_suffix"],
        )


class ProjectTree:
    """Represent a tree of project files."""

    def __init__(
        self,
        project: str,
        context: Optional[Dict[str, str]] = None,
        config: Optional[ProjectConfig] = None,
        build_path: Optional[str] = None,
        subdirs: Optional[str] = None,
        src_subdirs: Optional[str] = None,
        check_shared_lib: bool = True,
        absent_dir_error: bool = False,
        implicit_with: Optional[List[str]] = None,
        target: Optional[str] = None,
        language_runtimes: Optional[Dict[str, str]] = None,
    ) -> None:
        """Load a project tree.

        :param project: the root project file to load
        :param context: the external variables values. The key is the variable
            name. If None no external variables are passed.
        :param configuration: a gpr configuration
        """
        # Project existence should be handled by the GPR2 load function. But
        # currently the API returns a syntax error. Remove check when this is
        # fixed.
        if not os.path.isfile(project):
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

        self.id = LibGPR2.gpr2_prj_tree_load(
            request={
                "filename": project,
                "context": self._context,
                "configuration_id": self.config.id if self.config is not None else None,
                "build_path": self.build_path,
                "subdirs": self.subdirs,
                "src_subdirs": self.src_subdirs,
                "check_shared_lib": self.check_shared_lib,
                "implicit_with": self.implicit_with,
                "target": self.target,
                "language_runtime": self.language_runtimes,
            }
        )["tree_id"]

        self.messages = []
        self.properties_for_languages = {}
        self._update_properties()

    def _update_properties(self):
        properties = LibGPR2.gpr2_prj_tree_properties({"tree_id": self.id})
        self.target = properties["target"]
        self.archive_suffix = properties["archive_suffix"]

    def language_properties(self, language: str):
        return LanguageProperties.from_dict(
            language,
            LibGPR2.gpr2_prj_tree_language_properties(
                {"tree_id": self.id, "language": language}
            ),
        )

    @property
    def context(self):
        """Return the context of the tree.

        :return: the context
        """
        return self._context

    @context.setter
    def context(self, value: Dict[str, str]):
        LibGPR2.gpr2_prj_tree_set_context({"tree_id": self.id, "context": value})
        self._context = value
        self._update_properties()

    @property
    def root_view(self) -> ProjectView:
        """Return the root project view of the tree.

        :return: the root project view
        """
        answer = LibGPR2.gpr2_prj_tree_root_project({"tree_id": self.id})
        return ProjectView(id=answer["view_id"])

    def fetch_messages(self) -> List[Message]:
        """Fetch new messages.

        Note that unread messages are also added to self.messages

        :return: unread messages
        """
        answer = LibGPR2.gpr2_prj_tree_log_messages(
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
