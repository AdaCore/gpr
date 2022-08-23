#
#  Copyright (C) 2022, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations


class ProjectAttribute:
    """Represent a project attribute value."""

    def __init__(self, value, is_default: bool) -> None:
        self.value = value
        self.is_default = is_default
