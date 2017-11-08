from langkit.dsl import Field

from language.parser import A, GPRNode


class CompilationUnit(GPRNode):
    project = Field()


A.add_rules(

    compilation_unit=CompilationUnit(
        A.project
    ),

)
