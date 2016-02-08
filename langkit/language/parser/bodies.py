from langkit.compiled_types import Field, abstract
from langkit.parsers import List, Opt, Row, Or

from language.parser import A, GPRNode


class CompilationUnit(GPRNode):
    project = Field()


A.add_rules(

    compilation_unit=Row(
        A.project
    ) ^ CompilationUnit,

)
