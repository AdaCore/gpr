#
#  Copyright (C) 2020-2022, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from typing import TYPE_CHECKING
from gpr2.capi import LibGPR2
from gpr2.attr import ProjectAttribute
from gpr2.source import ProjectSource
import os

if TYPE_CHECKING:
    from gpr2.tree import ProjectTree
    from typing import Optional


class ProjectView:
    """A Project View.

    :ivar id: view id
    :ivar name: view name (i.e: gpr project name)
    :ivar path: path to the associated GPR project file
    :ivar dir: directory associated with the project file. By default this correspond
        to the directory of the GPR2 project file unless project_dir was used during
        loading of the tree.
    :ivar kind: view kind
    :ivar tree: corresponding tree
    """

    def __init__(self, tree: ProjectTree, id: str) -> None:
        self._view_data = LibGPR2.view_load({"tree_id": tree.id, "view_id": id})
        self.tree = tree
        self.id = self._view_data["id"]
        self.name = self._view_data["name"]
        self.dir = self._view_data["dir"]
        self.path = self._view_data["path"]
        self.kind = self._view_data["kind"]

    def attribute(
        self,
        name: str,
        pkg: Optional[str] = None,
        filename: Optional[str] = None,
        position: int = 0,
        language: Optional[str] = None,
        index: Optional[str] = None,
    ) -> str | list[str]:
        """Get final attribute value.

        :param name: the attribute name
        :param pkg: the package name
        :param filename: use a filename as index (if not None, language and index
            parameters are ignored). Note that only the basename part is considered even
            if a path containing directory information is passed.
        :param position: if filename is not None, the unix index inside the file pointed
            by filename (only relevant for Ada sources)
        :param language: use language as index. If not None index parameter is ignored.
            This is a case insensitive value
        :param index: use index as index (case sensitive)
        """
        params = {"tree_id": self.tree.id, "view_id": self.id, "name": name, "pkg": pkg}
        if filename is not None:
            params["filename"] = os.path.basename(filename)
            params["position"] = position
        elif language is not None:
            params["language"] = language
        elif index is not None:
            params["index"] = index

        data = LibGPR2.view_attribute(params)

        return ProjectAttribute(
            value=data["attribute"]["value"], is_default=data["attribute"]["is_default"]
        )

    def variables(self):
        return LibGPR2.gpr2_prj_view_variables({"view_id": self.id})

    def sources(self) -> list[ProjectSource]:
        """Get the list of sources for the given view.

        Note that if not already fetched the source list is computed on first call.

        :return: the list of Source part of the view
        """
        # By default first call to sources in GPR2 will cause a full parsing of the
        # sources with dependencies
        if not self.tree.have_source_list:
            self.tree.update_source_list()

        return [
            ProjectSource.from_dict(view=self, data=src)
            for src in LibGPR2.view_sources(
                {"tree_id": self.tree.id, "view_id": self.id}
            )["sources"]
        ]

    def units(self):
        print(
            LibGPR2.view_units({"tree_id": self.tree.id, "view_id": self.id})["units"]
        )
        return LibGPR2.gpr2_prj_view_units(
            {"tree_id": self.tree.id, "view_id": self.id}
        )["units"]
