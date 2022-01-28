from langkit.parsers import List
from langkit.dsl import Field, T

from language.parser import A, GprNode


class TypedStringDecl(GprNode):
    type_id = Field(type=T.Identifier)
    string_literals = Field(type=T.StringLiteral.list)


A.add_rules(
    typed_string_decl=TypedStringDecl(
        "type",
        A.identifier,
        "is",
        "(",
        List(A.string_literal, sep=","),
        ")",
        ";",
    ),
)
