from language.parser import A
from langkit.parsers import Opt, List, Or, Pick, Skip
from langkit.dsl import AbstractField, Field, T, abstract

from language.parser import GprNode
from language.parser.lexer import gpr_lexer as Lex


class AllQualifier(GprNode):
    enum_node = True
    qualifier = True


class Limited(GprNode):
    enum_node = True
    qualifier = True


class WithDecl(GprNode):
    is_limited = Field(type=T.Limited)
    path_names = Field(T.Expr.list)


class ProjectQualifier(GprNode):
    enum_node = True
    alternatives = [
        "abstract",  "standard", "library",
        "aggregate", "aggregate_library",
        "configuration",
    ]


class ProjectExtension(GprNode):
    is_all = Field(type=T.AllQualifier)
    path_name = Field(type=T.StringLiteral)


class ProjectDeclaration(GprNode):
    qualifier = Field(type=T.ProjectQualifier)
    project_name = Field(type=T.Expr)
    extension = Field(type=T.ProjectExtension)
    decls = Field(type=T.GprNode.list)
    end_name = Field(type=T.Expr)


class Project(GprNode):
    context_clauses = Field(type=T.WithDecl.list)
    project_decl = Field(type=T.ProjectDeclaration)


class VariableDecl(GprNode):
    var_name = Field(type=T.Identifier)
    var_type = Field(type=T.TypeReference)
    expr = Field(type=T.TermList)


class AttributeDecl(GprNode):
    attr_name = Field(type=T.Identifier)
    attr_index = Field(type=T.GprNode)
    expr = Field(type=T.TermList)


class PackageExtension(GprNode):
    extended_name = Field(type=T.Identifier.list)


class PackageDecl(GprNode):
    pkg_name = Field(type=T.Identifier)
    pkg_spec = Field(type=T.GprNode)


class PackageRenaming(GprNode):
    renamed_name = Field(type=T.Identifier.list)


class PackageSpec(GprNode):
    extension = Field(type=T.PackageExtension)
    decls = Field(type=T.GprNode.list)
    end_name = Field(type=T.Identifier)


class EmptyDecl(GprNode):
    pass


class CaseConstruction(GprNode):
    var_ref = Field(type=T.VariableReference)
    items = Field(type=T.CaseItem.list)


class CaseItem(GprNode):
    choice = Field(type=T.Choices)
    decls = Field(type=T.GprNode.list)


class OthersDesignator(GprNode):
    pass


class Choices(GprNode.list):
    pass


def l_id(text):
    return Lex.Identifier(text)


A.add_rules(
    project_qualifier=Or(
        ProjectQualifier.alt_abstract("abstract"),
        ProjectQualifier.alt_library(l_id("library")),

        ProjectQualifier.alt_aggregate_library(
            l_id("aggregate"), l_id("library")
        ),

        ProjectQualifier.alt_aggregate(l_id("aggregate")),

        ProjectQualifier.alt_configuration(l_id("configuration")),

        ProjectQualifier.alt_standard(l_id("standard")),
    ),
    project_extension=ProjectExtension(
        "extends", Opt("all").as_bool(AllQualifier), A.string_literal
    ),
    project_declaration=ProjectDeclaration(
        Opt(A.project_qualifier),
        l_id("project"),
        A.static_name,
        Opt(A.project_extension),
        "is",
        A.declarative_items,
        "end",
        A.static_name,
        ";",
    ),
    project=Project(A.context_clauses, A.project_declaration,),
    # ----------------------------------------------- declarative items
    declarative_items=List(A.declarative_item, empty_valid=True),
    declarative_item=Or(
        A.simple_declarative_item, A.typed_string_decl, A.package_decl
    ),
    simple_declarative_items=List(A.simple_declarative_item, empty_valid=True),
    simple_declarative_item=Or(
        A.variable_decl,
        A.attribute_decl,
        A.case_construction,
        A.empty_declaration,
    ),
    variable_decl=VariableDecl(
        A.identifier, Opt(Pick(":", A.type_reference)), ":=", A.expression, ";"
    ),
    attribute_decl=AttributeDecl(
        "for",
        A.identifier,
        Opt(Pick("(", A.associative_array_index, ")")),
        "use",
        A.expression,
        ";",
    ),
    associative_array_index=Or(A.others_designator, A.string_literal_at),
    package_decl=PackageDecl(
        "package", A.identifier, Or(A.package_renaming, A.package_spec), ";"
    ),
    package_renaming=PackageRenaming(
        "renames", List(A.identifier, sep=".")
    ),
    package_extension=PackageExtension(
        "extends", List(A.identifier, sep=".")
    ),
    package_spec=PackageSpec(
        Opt(A.package_extension),
        "is",
        A.simple_declarative_items,
        "end",
        A.identifier,
    ),
    empty_declaration=EmptyDecl("null", ";"),
    case_construction=CaseConstruction(
        "case",
        A.variable_reference,
        "is",
        List(A.case_item, empty_valid=True),
        "end",
        "case",
        ";",
    ),
    case_item=CaseItem(
        "when",
        A.discrete_choice_list,
        "=>",
        # ??? A.declarative_items
        A.simple_declarative_items,
    ),
    others_designator=OthersDesignator("others"),
    choice=Or(A.string_literal, A.others_designator),
    discrete_choice_list=List(A.choice, sep="|", list_cls=Choices),

    with_decl=WithDecl(
        Opt("limited").as_bool(Limited),
        "with",
        List(A.string_literal, sep=","),
        ";",
    ),
    context_clauses=List(A.with_decl, empty_valid=True),
)
