#
#  Copyright (C) 2020-2026, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from typing import TYPE_CHECKING
from gpr2.capi import LibGPR2
from gpr2.source import ProjectSource

if TYPE_CHECKING:
    from gpr2.tree import ProjectTree
    from typing import Optional


class ProjectView:

    def __init__(self, tree: ProjectTree, id: str) -> None:
        """A Project View.

        :ivar id: view id
        :ivar tree: corresponding tree
        """
        self.id = id
        self.tree = tree

        answer = LibGPR2.view_constructor(request={"view_id": self.id})

        self.name = answer["name"]
        self.path_name = answer["path_name"]

    def __del__(self):
        LibGPR2.view_destructor(request={"view_id": self.id})

    @property
    def executables(self) -> list[string]:
        """Returns the mains's binary full pathname"""
        answer = LibGPR2.view_executables(request={"view_id": self.id})
        return answer["executables"]

    @property
    def object_directory(self) -> string | None:
        """
        Returns the object directory, note that this may be different than
        getting the Object_Dir attribute value as the result here is always
        a path-name with proper resolution for relative directory specification.
        """
        answer = LibGPR2.view_object_directory(request={"view_id": self.id})
        return answer["object_directory"]

    @property
    def sources(self) -> list[ProjectSource]:
        """Get the list of sources for the given view.

        Note that if not already fetched the source list is computed on first call.

        :return: the list of Source part of the view
        """
        answer = LibGPR2.view_sources(request={"view_id": self.id})
        return [
            ProjectSource.from_dict(view=self, data=src) for src in answer["sources"]
        ]

    @property
    def visible_sources(self) -> list[ProjectSource]:
        """Get the list of visible sources for the given view.

        :return: the list of Source part of the view
        """
        answer = LibGPR2.view_visible_sources(request={"view_id": self.id})
        return [
            ProjectSource.from_dict(view=self, data=src)
            for src in answer["visible_sources"]
        ]


class ProjectViewIterator:
    def __init__(self, views: list[ProjectView]):
        self._views = views
        self._index = 0

    def __next__(self):
        if self._index < len(self._views):
            self._index += 1
            return self._views[self._index - 1]

        else:
            raise StopIteration
