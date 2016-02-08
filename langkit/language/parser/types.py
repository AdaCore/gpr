from langkit.parsers import List, Opt, Row, Or
from langkit.compiled_types import Field, abstract

from language.parser import A, GPRNode


class TypedStringDecl(GPRNode):
    type_id = Field()
    string_literals = Field()


A.add_rules(
    typed_string_decl=Row(
        "type", A.identifier, "is",
        "(", List(A.string_literal, sep=","), ")"
    ) ^ TypedStringDecl,

)
