from langkit.parsers import Opt, List, Or, Row, Tok
from langkit.dsl import Annotations, Field, abstract

from language.parser import A, GPRNode
from language.parser.lexer import Token


@abstract
class Expr(GPRNode):
    pass


@abstract
class SingleTokNode(Expr):
    tok = Field()


class Identifier(SingleTokNode):
    annotations = Annotations(repr_name='Id')


class StringLiteral(SingleTokNode):
    annotations = Annotations(repr_name='Str')


class NumLiteral(SingleTokNode):
    annotations = Annotations(repr_name='Num')


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


class BuiltinFunctionCall(GPRNode):
    function_name = Field()
    parameters = Field()


class VariableReference(GPRNode):
    variable_name1 = Field()
    variable_name2 = Field()
    attribute_ref = Field()


class ProjectReference(GPRNode):
    attr_ref = Field()


A.add_rules(
    identifier=Tok(Token.Identifier, keep=True) ^ Identifier,
    string_literal=Tok(Token.String, keep=True) ^ StringLiteral,
    num_literal=Tok(Token.Number, keep=True) ^ NumLiteral,

    static_name=Or(A.identifier,
                   Prefix(A.static_name, '.', A.identifier)),
    # ----------------------------------------------------------------

    attribute_reference=Row(
        A.identifier,
        Opt(Row("(", Or(A.others_designator, A.string_literal), ")")[1])
    ) ^ AttributeReference,

    variable_reference=Row(
        A.identifier,
        Opt(Row(".", A.identifier)[1]),  # JM added???
        Opt(Row("'", A.attribute_reference)[1])
    ) ^ VariableReference,

    builtin_function_call=Row(
        A.identifier,
        A.expression_list
    ) ^ BuiltinFunctionCall,
    # ----------------------------------------------------------------

    expression=List(A.term, sep="&") ^ TermList,

    expression_list=Row(
        "(", List(A.expression, sep=",", empty_valid=True), ")") ^ ExprList,

    string_literal_at=Row(
        A.string_literal,
        Opt(Row("at", A.num_literal)[1])
    ) ^ StringLiteralAt,

    project_reference=Row(
        Tok("project"), "'", A.attribute_reference
    ) ^ ProjectReference,

    term=Or(
        A.expression_list,
        A.string_literal_at,
        A.builtin_function_call,
        A.variable_reference,
        A.project_reference
    ),

)
