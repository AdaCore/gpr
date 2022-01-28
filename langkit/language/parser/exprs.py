from langkit.parsers import Opt, List, Or, Pick
from langkit.dsl import Annotations, Field, T, abstract

from language.parser import A, GprNode
from language.parser.lexer import Token, gpr_lexer as Lex


@abstract
class Expr(GprNode):
    pass


@abstract
class SingleTokNode(Expr):
    token_node = True


class Identifier(SingleTokNode):
    annotations = Annotations(repr_name="Id")


class StringLiteral(SingleTokNode):
    annotations = Annotations(repr_name="Str")


class NumLiteral(SingleTokNode):
    annotations = Annotations(repr_name="Num")


class Prefix(Expr):
    prefix = Field(type=T.Expr)
    suffix = Field(type=T.Identifier)


class TermList(GprNode.list):
    pass


class Terms(GprNode):
    terms = Field(type=T.TermList.list)


class StringLiteralAt(GprNode):
    str_lit = Field(type=T.StringLiteral)
    at_lit = Field(type=T.NumLiteral)


class AttributeReference(GprNode):
    attribute_name = Field(type=T.Identifier)
    attribute_index = Field(type=T.GprNode)


class BuiltinFunctionCall(GprNode):
    function_name = Field(type=T.Identifier)
    parameters = Field(type=Terms)


class VariableReference(GprNode):
    variable_name = Field(type=T.Identifier.list)
    attribute_ref = Field(type=T.AttributeReference)


class TypeReference(GprNode):
    var_type_name = Field(type=T.Identifier.list)


class ProjectReference(GprNode):
    attr_ref = Field(type=T.AttributeReference)


A.add_rules(
    identifier=Identifier(Token.Identifier),
    string_literal=StringLiteral(Token.String),
    num_literal=NumLiteral(Token.Number),
    static_name=Or(Prefix(A.static_name, ".", A.identifier), A.identifier),
    # ----------------------------------------------------------------
    attribute_reference=AttributeReference(
        A.identifier,
        Opt(Pick("(", Or(A.others_designator, A.string_literal), ")")),
    ),
    variable_reference=VariableReference(
        List(A.identifier, sep="."),
        Opt(Pick("'", A.attribute_reference)),
    ),
    type_reference=TypeReference(List(A.identifier, sep="."),),
    builtin_function_call=BuiltinFunctionCall(A.identifier, A.expression_list),
    # ----------------------------------------------------------------
    expression=List(A.term, sep="&", list_cls=TermList),
    expression_list=Terms(
        "(", List(A.expression, sep=",", empty_valid=True), ")"
    ),
    string_literal_at=StringLiteralAt(
        A.string_literal, Opt(Pick("at", A.num_literal))
    ),
    project_reference=ProjectReference(
        Lex.Identifier(match_text="project"), "'", A.attribute_reference
    ),
    term=Or(
        A.expression_list,
        A.string_literal_at,
        A.builtin_function_call,
        A.variable_reference,
        A.project_reference,
    ),
)
