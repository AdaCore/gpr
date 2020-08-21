from __future__ import annotations
from typing import TYPE_CHECKING

from gpr2.capi import LibGPR2

if TYPE_CHECKING:
    from types import TracebackType
    from typing import Optional, Type


class ProjectView:
    def __init__(self, id: str):
        self.id = id
        self._update_properties()

    def _update_properties(self):
        properties = LibGPR2.gpr2_prj_view_properties({"view_id": self.id})
        self.name = properties["name"]
        self.path_name = properties["path_name"]
        self.dir_name = properties["dir_name"]
        self.qualifier = properties["qualifier"]
        self.kind = properties["kind"]

    def attributes(self):
        return LibGPR2.gpr2_prj_view_attributes({"view_id": self.id})

    def variables(self):
        return LibGPR2.gpr2_prj_view_variables({"view_id": self.id})

    def sources(self):
        return LibGPR2.gpr2_prj_view_sources({"view_id": self.id})

    def close(self) -> None:
        """Unload a project view."""
        LibGPR2.gpr2_prj_view_unload({"view_id": self.id})
        self.id = None

    def __enter__(self) -> ProjectView:
        return self

    def __exit__(
        self,
        _type: Optional[Type[BaseException]],
        _val: Optional[BaseException],
        _tb: Optional[TracebackType],
    ) -> None:
        self.close()
