from langkit.parsers import List
from langkit.dsl import Field

from language.parser import A, GPRNode


class TypedStringDecl(GPRNode):
    type_id = Field()
    string_literals = Field()


A.add_rules(
    typed_string_decl=TypedStringDecl(
        "type", A.identifier, "is",
        "(", List(A.string_literal, sep=","), ")"
    ),

)
