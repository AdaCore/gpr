from langkit.dsl import Field, T

from language.parser import A, GPRNode


class CompilationUnit(GPRNode):
    project = Field(type=T.Project)


A.add_rules(compilation_unit=CompilationUnit(A.project),)
