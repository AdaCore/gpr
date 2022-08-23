#
#  Copyright (C) 2022, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from datetime import datetime
from typing import TYPE_CHECKING
from gpr2.capi import LibGPR2

if TYPE_CHECKING:
    from gpr2.view import ProjectView


class ProjectSource:
    """A source from the project tree."""

    def __init__(
        self,
        view: str,
        path: str,
        is_aggregated: bool,
        is_compilable: bool,
        is_interface: bool,
        has_name_exception: bool,
        is_main: bool,
        language: str,
        timestamp: datetime,
    ) -> None:
        """Initialize a source.

        :param view: the view the source belong to
        :param path: path to the source file
        :param is_aggregated: whether the source is part of an aggregated library
        :param is_compilable: whether the source is compilable. It means whether it
            makes sense to launch the compiler on that source. Note that even if
            compilable is True, the source should not be compiled in case the
            attribute is_externally_built is set to "true". Indeed in that case
            the user might not have the information necessary to compile the given
            source
        :param is_interface: if True means the source is part of the library interface
        :param has_name_exception: if True then the source comes from a naming
            exception
        :param is_main: if True the source is the main file for an executable
        :param language: the source language
        :param timestamp: last modification time for the source
        """
        self.view = view
        self.path = path
        self.is_aggregated = is_aggregated
        self.is_compilable = is_compilable
        self.is_interface = is_interface
        self.has_name_exception = has_name_exception
        self.is_main = is_main
        self.language = language
        self.timestamp = timestamp
        self._dependencies = None

    @classmethod
    def from_dict(cls, view: ProjectView, data: dict) -> ProjectSource:
        """Load a Source from json data."""
        return cls(
            view=view,
            path=data["path"],
            is_aggregated=data["is_aggregated"],
            is_compilable=data["is_compilable"],
            is_interface=data["is_interface"],
            has_name_exception=data["has_name_exception"],
            is_main=data["is_main"],
            language=data["language"],
            timestamp=datetime.utcfromtimestamp(data["timestamp"]),
        )

    @property
    def dependencies(self) -> list[ProjectSource]:
        """Return the list of dependencies for the current source.

        :return: the list of source dependencies
        """
        if self._dependencies is None:
            self.update_dependencies()
        return self._dependencies

    def update_dependencies(self, allow_source_parsing: bool = False) -> None:
        """Update source dependencies.

        :param allow_source_parsing: enable source parsers if available
        """
        result = LibGPR2.source_update_source_infos(
            {
                "tree_id": self.view.tree.id,
                "view_id": self.view.id,
                "path": self.path,
                "allow_source_parsing": allow_source_parsing,
            }
        )
        self._dependencies = [
            ProjectSource.from_dict(self.view, s) for s in result["dependencies"]
        ]

    def __str__(self) -> str:
        return f"{self.path} (lang={self.language} time={self.timestamp})"
