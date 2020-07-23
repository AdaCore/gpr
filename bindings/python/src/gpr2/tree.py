from __future__ import annotations
from gpr2 import GPR2Error
from gpr2.capi import LibGPR2
from gpr2.view import ProjectView
from gpr2.level_format import LevelFormat
from gpr2.message import Message
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

        request = {"view_id": self.id}
        self.properties = LibGPR2.gpr2_prj_tree_properties(request)

    @property
    def context(self):
        """Return the context of the tree.

        :return: the context
        """
        answer = LibGPR2.gpr2_prj_tree_context({"tree_id": self.id})
        context = {}
        for element in answer['context']:
            context[element['name']] = element['value']
        return context

    @property
    def root_view(self) -> ProjectView:
        """Return the root project view of the tree.

        :return: the root project view
        """
        answer = LibGPR2.gpr2_prj_tree_root_project({"tree_id": self.id})
        return ProjectView(id=answer["view_id"])

    def log_messages(self,
                     information = True,
                     warning = True,
                     error = True,
                     read = True,
                     unread = True,
                     full_path_name = False,
                     information_output_level = LevelFormat.LONG,
                     warning_output_level = LevelFormat.LONG,
                     error_output_level = LevelFormat.LONG):
        """Return the messages in tree's log.
        use information/warning/error/read/unread parameters to filter messages
        use others parameters to get expected formatted_message

        :return: the Message objects array
        """
        answer = LibGPR2.gpr2_prj_tree_log_messages(
            {"tree_id": self.id,
             "information": information,
             "warning": warning,
             "error": error,
             "read": read,
             "unread": unread,
             "information_output_level": information_output_level,
             "warning_output_level": warning_output_level,
             "error_output_level": error_output_level,
             })
        messages = []
        for message in answer['messages']:
            messages.append(Message(message=message))
        return messages

    @property
    def target(self):
        """Returns the target for the project tree

        :return: the target
        """
        return properties["target"]

    @property
    def build_path(self):
        """Returns the build path for the project tree

        :return: the build path
        """
        return properties["build_path"]

    @property
    def archive_suffix(self):
        """Returns the archive suffix for the project tree

        :return: the archive suffix
        """
        return properties["archive_suffix"]

    @property
    def subdirs(self):
        """Returns the subdirs for the project tree

        :return: the subdirs
        """
        return properties["subdirs"]

    @property
    def src_subdirs(self):
        """Returns the src subdirs for the project tree

        :return: the src subdirs
        """
        return properties["src_subdirs"]

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
