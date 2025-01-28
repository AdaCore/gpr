import datetime
from langkit.utils import SourcePostProcessor


copyright_lines = [
    f"Copyright (C) 2019-{datetime.date.today().year}, AdaCore",
    "SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception",
]

ada_header = f"""\
--
--  {copyright_lines[0]}
--  {copyright_lines[1]}
--
"""

c_header = f"""\
/*
--  {copyright_lines[0]}
--  {copyright_lines[1]}
*/
"""


class AdaPostProcessor(SourcePostProcessor):
    def process(self, source):
        return ada_header + source


class CPostProcessor(SourcePostProcessor):
    def process(self, source):
        return c_header + source
