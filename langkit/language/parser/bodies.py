from langkit.dsl import Field, T

from language.parser import A, GprNode


class CompilationUnit(GprNode):
    project = Field(type=T.Project)


A.add_rules(compilation_unit=CompilationUnit(A.project),)
