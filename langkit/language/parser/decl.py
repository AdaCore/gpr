from language.parser import A
from langkit.parsers import Opt, List, Or, Pick, Skip
from langkit.dsl import AbstractField, Field, T, abstract

from language.parser import GprNode
from language.parser.lexer import gpr_lexer as Lex


class AllQualifier(GprNode):
    enum_node = True
    qualifier = True


class Private(GprNode):
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


@abstract
class AdaPreludeNode(GprNode):
    pass


@abstract
class AdaContextClause(AdaPreludeNode):
    pass


class AdaSkip(AdaPreludeNode):
    error_node = True


class AdaUse(AdaContextClause):
    skips = Field(type=T.AdaSkip.list)


class AdaPragma(AdaContextClause):
    skips = Field(type=T.AdaSkip.list)


class AdaWith(AdaContextClause):
    has_limited = Field(type=T.Limited)
    has_private = Field(type=T.Private)
    packages = Field(T.Expr.list)


@abstract
class AdaMain(AdaPreludeNode):
    name = AbstractField(type=T.Expr)


class AdaEntityKind(AdaPreludeNode):
    enum_node = True
    alternatives = ["procedure", "function", "package"]


class AdaSubp(AdaMain):
    subp_kind = Field(type=AdaEntityKind)
    name = Field(type=T.Expr)


class AdaPkg(AdaMain):
    has_private = Field(type=T.Private)
    name = Field(type=T.Expr)


class AdaPkgBody(AdaMain):
    name = Field(type=T.Expr)


class AdaLibraryItem(AdaPreludeNode):
    generic_stub = Field(type=T.AdaGeneric)
    separate = Field(type=T.AdaSeparate)
    main = Field(type=T.AdaMain)


class AdaSeparate(AdaPreludeNode):
    parent_name = Field(type=T.Expr)


class AdaGeneric(AdaPreludeNode):
    skips = Field(type=T.GprNode)


class AdaWithFormal(AdaPreludeNode):
    kind = Field(T.AdaEntityKind)
    skips = Field(type=T.AdaSkip.list)


class AdaAccessSubp(AdaPreludeNode):
    subp_kind = Field(type=AdaEntityKind)
    skips = Field(type=T.AdaSkip.list)


class AdaPrelude(AdaPreludeNode):
    context_clauses = Field(type=T.AdaContextClause.list)
    library_item = Field(type=T.AdaLibraryItem)


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

    # Ada context clauses parsing
    #########################################
    ada_with_clause=AdaWith(
        Limited("limited"),
        Private("private"),
        "with",
        List(A.static_name, sep=","),
        ";",
    ),
    ada_context=List(A.ada_context_item, empty_valid=True),
    ada_context_item=Or(A.ada_with_clause, A.ada_use_clause, A.ada_pragma),
    ada_context_skip=List(Or(Skip(AdaSkip),)),
    ada_use_clause=AdaUse("use", A.ada_context_skip.dont_skip(";"), ";"),
    ada_pragma=AdaPragma("pragma", A.ada_context_skip.dont_skip(";"), ";"),
    ada_subp_kind=Or(AdaEntityKind.alt_procedure(l_id("procedure")),
                     AdaEntityKind.alt_function(l_id("function"))),
    ada_pkg_kind=AdaEntityKind.alt_package("package"),
    ada_library_item=AdaLibraryItem(

        Opt(AdaGeneric(
            l_id("generic"),

            List(Or(
                # Parse formals that start with "with".
                AdaWithFormal(
                    "with",

                    # We want to parse the subprogram or package's starting
                    # keyword explicitly, because it cannot be skipped.
                    Or(A.ada_subp_kind, A.ada_pkg_kind),

                    A.ada_context_skip.dont_skip(";")
                ),

                # Parse accesses to subprograms (we need to handle those
                # explicitly for the same reason as above, because they cannot
                # be skipped since they contain "function"/"procedure"
                AdaAccessSubp(l_id("access"),
                              A.ada_subp_kind,
                              A.ada_context_skip.dont_skip(";")),

                Skip(AdaSkip),
            ), empty_valid=True)

            # Don't skip procedure/function/package in the general case since
            # that's the start of the library item we're looking for.
            .dont_skip(Or(l_id('procedure'), l_id('function'), 'package'))

        )),

        Opt(AdaSeparate(l_id("separate"), "(", A.static_name, ")")),

        Or(
            AdaSubp(A.ada_subp_kind, A.static_name),
            AdaPkgBody("package", l_id("body"), A.static_name),
            AdaPkg(
                Private("private"), "package", A.static_name
            ),
        )
    ),
    ada_prelude=AdaPrelude(A.ada_context, A.ada_library_item),
    # end Ada with clauses parsing
    #########################################

)
