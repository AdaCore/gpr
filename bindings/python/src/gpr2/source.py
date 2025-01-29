#
#  Copyright (C) 2022-2025, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from gpr2.view import ProjectView


class ProjectSource:
    """A source from the project tree."""

    def __init__(
        self,
        view: str,
        language: str,
        path_name: str,
    ) -> None:
        """Initialize a source.

        :param view: the view the source belong to
        :param path_name: path to the source file
        :param language: the source language
        """
        self.view = view
        self.language = language
        self.path_name = path_name

    @classmethod
    def from_dict(cls, view: ProjectView, data: dict) -> ProjectSource:
        """Load a Source from json data."""
        return cls(
            view=view,
            language=data["language"],
            path_name=data["path_name"],
        )
