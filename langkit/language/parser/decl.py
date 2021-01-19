from language.parser import A
from langkit.parsers import Opt, List, Or, Pick, Skip
from langkit.dsl import AbstractField, Field, T, abstract

from language.parser import GPRNode
from language.parser.lexer import gpr_lexer as Lex


class AllQualifier(GPRNode):
    enum_node = True
    qualifier = True


class Private(GPRNode):
    enum_node = True
    qualifier = True


class Limited(GPRNode):
    enum_node = True
    qualifier = True


class WithDecl(GPRNode):
    is_limited = Field(type=T.Limited)
    path_names = Field(T.Expr.list)


class ProjectQualifier(GPRNode):
    enum_node = True
    alternatives = [
        "abstract",  "standard", "library",
        "aggregate", "aggregate_library",
        "configuration",
    ]


class ProjectExtension(GPRNode):
    is_all = Field(type=T.AllQualifier)
    path_name = Field(type=T.StringLiteral)


class ProjectDeclaration(GPRNode):
    qualifier = Field(type=T.ProjectQualifier)
    project_name = Field(type=T.Expr)
    extension = Field(type=T.ProjectExtension)
    decls = Field(type=T.GPRNode.list)
    end_name = Field(type=T.Expr)


class Project(GPRNode):
    context_clauses = Field(type=T.WithDecl.list)
    project_decl = Field(type=T.ProjectDeclaration)


class VariableDecl(GPRNode):
    var_name = Field(type=T.Identifier)
    var_type = Field(type=T.TypeReference)
    expr = Field(type=T.TermList)


class AttributeDecl(GPRNode):
    attr_name = Field(type=T.Identifier)
    attr_index = Field(type=T.GPRNode)
    expr = Field(type=T.TermList)


class PackageExtension(GPRNode):
    prj_name = Field(type=T.Identifier)
    pkg_name = Field(type=T.Identifier)


class PackageDecl(GPRNode):
    pkg_name = Field(type=T.Identifier)
    pkg_spec = Field(type=T.GPRNode)


class PackageRenaming(GPRNode):
    prj_name = Field(type=T.Identifier)
    pkg_name = Field(type=T.Identifier)


class PackageSpec(GPRNode):
    extension = Field(type=T.PackageExtension)
    decls = Field(type=T.GPRNode.list)
    end_name = Field(type=T.Identifier)


class EmptyDecl(GPRNode):
    pass


class CaseConstruction(GPRNode):
    var_ref = Field(type=T.VariableReference)
    items = Field(type=T.CaseItem.list)


class CaseItem(GPRNode):
    choice = Field(type=T.Choices)
    decls = Field(type=T.GPRNode.list)


class OthersDesignator(GPRNode):
    pass


class Choices(GPRNode.list):
    pass


@abstract
class AdaPreludeNode(GPRNode):
    pass


@abstract
class AdaContextClause(AdaPreludeNode):
    pass


class AdaSkip(AdaPreludeNode):
    pass


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


class AdaSubpKind(AdaPreludeNode):
    enum_node = True
    alternatives = ["procedure", "function"]


class AdaSubp(AdaMain):
    subp_kind = Field(type=AdaSubpKind)
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
    skips = Field(type=T.GPRNode)


class AdaWithFormal(AdaPreludeNode):
    skips = Field(type=T.AdaSkip.list)


class AdaAccessSubp(AdaPreludeNode):
    subp_kind = Field(type=AdaSubpKind)
    skips = Field(type=T.AdaSkip.list)


class AdaPrelude(AdaPreludeNode):
    context_clauses = Field(type=T.AdaContextClause.list)
    library_item = Field(type=T.AdaLibraryItem)


A.add_rules(
    project_qualifier=Or(
        ProjectQualifier.alt_abstract("abstract"),
        ProjectQualifier.alt_library(Lex.Identifier("library")),

        ProjectQualifier.alt_aggregate_library(
            Lex.Identifier("aggregate"), Lex.Identifier("library")
        ),

        ProjectQualifier.alt_aggregate(Lex.Identifier("aggregate")),

        ProjectQualifier.alt_configuration(Lex.Identifier("configuration")),

        ProjectQualifier.alt_standard(Lex.Identifier("standard")),
    ),
    project_extension=ProjectExtension(
        "extends", Opt("all").as_bool(AllQualifier), A.string_literal
    ),
    project_declaration=ProjectDeclaration(
        Opt(A.project_qualifier),
        Lex.Identifier(match_text="project"),
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
        "renames", A.identifier, ".", A.identifier
    ),
    package_extension=PackageExtension(
        "extends", A.identifier, ".", A.identifier,
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
    ada_subp_kind=Or(AdaSubpKind.alt_procedure(Lex.Identifier("procedure")),
                     AdaSubpKind.alt_function(Lex.Identifier("function"))),
    ada_library_item=AdaLibraryItem(

        Opt(AdaGeneric(
            Lex.Identifier(match_text="generic"),

            List(Or(
                AdaWithFormal("with", A.ada_context_skip.dont_skip(";")),
                AdaAccessSubp(Lex.Identifier(match_text="access"),
                              A.ada_subp_kind,
                              A.ada_context_skip.dont_skip(";")),
                Skip(AdaSkip),
            ), empty_valid=True).dont_skip(
                Or(Lex.Identifier('procedure'),
                   Lex.Identifier('function'), 'package')
            )
        )),

        Opt(AdaSeparate(Lex.Identifier("separate"), "(", A.static_name, ")")),

        Or(
            AdaSubp(A.ada_subp_kind, A.static_name),
            AdaPkgBody("package", Lex.Identifier("body"), A.static_name),
            AdaPkg(
                Private("private"), "package", A.static_name
            ),
        )
    ),
    ada_prelude=AdaPrelude(A.ada_context, A.ada_library_item),
    # end Ada with clauses parsing
    #########################################

)
