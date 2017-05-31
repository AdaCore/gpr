from langkit.dsl import Field
from langkit.parsers import Row

from language.parser import A, GPRNode


class CompilationUnit(GPRNode):
    project = Field()


A.add_rules(

    compilation_unit=Row(
        A.project
    ) ^ CompilationUnit,

)
