from langkit.parsers import Opt, List, Or, Row, Enum, Tok, Null
from langkit.compiled_types import Field, abstract, EnumType

from language.parser import A, GPRNode
from language.parser.lexer import Token


@abstract
class Expr(GPRNode):
    pass


@abstract
class SingleTokNode(Expr):
    tok = Field()


class Identifier(SingleTokNode):
    _repr_name = "Id"


class StringLiteral(SingleTokNode):
    _repr_name = "Str"


class NumLiteral(SingleTokNode):
    _repr_name = "Num"


class Prefix(Expr):
    prefix = Field()
    suffix = Field()


class TermList(GPRNode):
    terms = Field()


class ExprList(GPRNode):
    exprs = Field()


class StringLiteralAt(GPRNode):
    str_lit = Field()
    at_lit = Field()


class AttributeReference(GPRNode):
    attribute_name = Field()
    attribute_index = Field()


class VariableReference(GPRNode):
    variable_name1 = Field()
    variable_name2 = Field()
    attribute_ref = Field()


class ProjectReference(GPRNode):
    attr_ref = Field()


class External(GPRNode):
    pass


class ExternalAsList(GPRNode):
    pass


class ExternalReference(GPRNode):
    kind = Field()
    string_lit = Field()
    expr = Field()


A.add_rules(
    identifier=Tok(Token.Identifier, keep=True) ^ Identifier,
    string_literal=Tok(Token.String, keep=True) ^ StringLiteral,
    num_literal=Tok(Token.Number, keep=True) ^ NumLiteral,

    static_name=List(A.identifier, sep=".", revtree=Prefix),
    # ----------------------------------------------------------------

    attribute_reference=Row(
        A.identifier,
        Opt("(", Or(A.others_designator, A.string_literal), ")")[1]
    ) ^ AttributeReference,

    variable_reference=Row(
        A.identifier,
        Opt(Row(".", A.identifier)[1]),  # JM added???
        Opt(Row("'", A.attribute_reference)[1])
    ) ^ VariableReference,

    external=Row(
        "external"
    ) ^ External,

    external_as_list=Row(
        "external_as_list"
    ) ^ ExternalAsList,

    external_reference=Row(
        Or(A.external, A.external_as_list),
        "(",
        A.string_literal,
        Opt(Row(",", A.expression)[1]),
        ")"
    ) ^ ExternalReference,
    # ----------------------------------------------------------------

    expression=List(A.term, sep="&") ^ TermList,

    expression_list=Row(
        "(", List(A.expression, sep=",", empty_valid=True), ")") ^ ExprList,

    string_literal_at=Row(
        A.string_literal,
        Opt(Row("at", A.num_literal)[1])
    ) ^ StringLiteralAt,

    project_reference=Row(
        "project", "'", A.attribute_reference
    ) ^ ProjectReference,

    term=Or(
        A.expression_list,
        A.string_literal_at,
        A.variable_reference,
        A.project_reference,
        A.external_reference
    ),

)
