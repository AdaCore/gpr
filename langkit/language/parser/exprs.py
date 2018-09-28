from langkit.parsers import Opt, List, Or, Pick
from langkit.dsl import Annotations, Field, abstract

from language.parser import A, GPRNode
from language.parser.lexer import Token


@abstract
class Expr(GPRNode):
    pass


@abstract
class SingleTokNode(Expr):
    token_node = True


class Identifier(SingleTokNode):
    annotations = Annotations(repr_name='Id')


class StringLiteral(SingleTokNode):
    annotations = Annotations(repr_name='Str')


class NumLiteral(SingleTokNode):
    annotations = Annotations(repr_name='Num')


class Prefix(Expr):
    prefix = Field()
    suffix = Field()


class TermList(GPRNode.list):
    pass


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
    variable_name3 = Field()
    attribute_ref = Field()


class TypeReference(GPRNode):
    var_type_name1 = Field()
    var_type_name2 = Field()


class ProjectReference(GPRNode):
    attr_ref = Field()


A.add_rules(
    identifier=Identifier(Token.Identifier),
    string_literal=StringLiteral(Token.String),
    num_literal=NumLiteral(Token.Number),


    static_name=Or(Prefix(A.static_name, '.', A.identifier),
                   A.identifier),
    # ----------------------------------------------------------------

    attribute_reference=AttributeReference(
        A.identifier,
        Opt(Pick("(", Or(A.others_designator, A.string_literal), ")"))
    ),

    variable_reference=VariableReference(
        A.identifier,
        Opt(Pick(".", A.identifier)),
        Opt(Pick(".", A.identifier)),
        Opt(Pick("'", A.attribute_reference))
    ),

    type_reference=TypeReference(
        A.identifier,
        Opt(Pick(".", A.identifier)),
    ),

    builtin_function_call=BuiltinFunctionCall(
        A.identifier,
        A.expression_list
    ),
    # ----------------------------------------------------------------

    expression=List(A.term, sep="&", list_cls=TermList),

    expression_list=ExprList(
        "(", List(A.expression, sep=",", empty_valid=True), ")"),

    string_literal_at=StringLiteralAt(
        A.string_literal,
        Opt(Pick("at", A.num_literal))
    ),

    project_reference=ProjectReference(
        "project", "'", A.attribute_reference
    ),

    term=Or(
        A.expression_list,
        A.string_literal_at,
        A.builtin_function_call,
        A.variable_reference,
        A.project_reference
    ),

)
