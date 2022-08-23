#
#  Copyright (C) 2020, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import Optional


class Sloc:
    """Represent a location in a file."""

    def __init__(
        self, filename: str, line: Optional[int] = None, column: Optional[int] = None
    ):
        """Initialize a SLOC instance.

        :param filename: filename
        :param line: line in the file (might be None)
        :param column: column in the file (might be None)
        """
        self.filename = filename
        self.line = line
        self.column = column

    @classmethod
    def from_dict(cls, data):
        """Create a Sloc instance from a dict.

        :param data: dict as returned by json.loads.
        """
        return cls(
            filename=data["filename"], line=data.get("line"), column=data.get("column")
        )

    def __str__(self):
        line_str = self.line if self.line else ""
        column_str = self.column if self.column else ""
        return f"{self.filename}:{line_str}:{column_str}"


class Message:
    """Represent a GPR2 message."""

    def __init__(self, message: str, level: str, sloc: Sloc):
        """Initialize a Message instance.

        :param message: message content
        :param level: message level
        :param sloc: file location associated with the message
        """
        self.level = level
        self.message = message
        self.sloc = sloc

    @classmethod
    def from_dict(cls, data):
        """Create a Message instance from a dict.

        :param data: dict as returned by json.loads
        """
        return cls(
            message=data["message"],
            level=data["level"],
            sloc=Sloc.from_dict(data["sloc"]),
        )

    def __str__(self):
        return f"{self.sloc}:{self.level}:{self.message}"
