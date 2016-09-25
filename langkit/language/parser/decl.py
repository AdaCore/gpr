from language.parser import A
from langkit.parsers import Opt, List, Or, Row, _, Enum, Tok
from langkit.compiled_types import Field, EnumType, abstract

from language.parser import GPRNode


class WithDecl(GPRNode):
    is_limited = Field()
    path_names = Field()


class QualifierNames(GPRNode):
    qualifier_id1 = Field()
    qualifier_id2 = Field()


class ProjectQualifier(GPRNode):
    qualifier = Field()


class ProjectExtension(GPRNode):
    is_all = Field()
    path_name = Field()


class ProjectDeclaration(GPRNode):
    qualifier = Field()
    project_name = Field()
    extension = Field()
    decls = Field()
    end_name = Field()


class Project(GPRNode):
    context_clauses = Field()
    project_decl = Field()


class VariableDecl(GPRNode):
    var_name = Field()
    var_type = Field()
    expr = Field()


class AttributeDecl(GPRNode):
    attr_name = Field()
    attr_index = Field()
    expr = Field()


class PackageExtension(GPRNode):
    prj_name = Field()
    pkg_name = Field()


class PackageDecl(GPRNode):
    pkg_name = Field()
    pkg_spec = Field()


class PackageRenaming(GPRNode):
    prj_name = Field()
    pkg_name = Field()


class PackageSpec(GPRNode):
    extension = Field()
    decls = Field()
    end_name = Field()


class EmptyDecl(GPRNode):
    pass


class CaseConstruction(GPRNode):
    var_ref = Field()
    items = Field()


class CaseItem(GPRNode):
    choice = Field()
    decls = Field()


class AbstractPresent(GPRNode):
    pass


class OthersDesignator(GPRNode):
    pass


A.add_rules(
    context_clauses=List(Row(A.with_decl, ";")[0], empty_valid=True),

    with_decl=Row(
        Opt("limited").as_bool(),
        "with", List(A.string_literal, sep=",")
    ) ^ WithDecl,

    abstract_present=Row(
        "abstract"
    ) ^ AbstractPresent,

    qualifier_names=Row(
        A.identifier, Opt(A.identifier)
    ) ^ QualifierNames,

    project_qualifier=Or(
        A.abstract_present,
        A.qualifier_names
    ) ^ ProjectQualifier,

    project_extension=Row(
        "extends", Opt("all").as_bool(), A.string_literal
    ) ^ ProjectExtension,

    project_declaration=Row(
        Opt(A.project_qualifier),
        "project",
        A.static_name,
        Opt(A.project_extension),
        "is",
        A.declarative_items,
        "end", A.static_name, ";"
    ) ^ ProjectDeclaration,

    project=Row(
        A.context_clauses,
        A.project_declaration,
    ) ^ Project,

    # ----------------------------------------------- declarative items

    declarative_items=List(
        Row(A.declarative_item, ";")[0], empty_valid=True),

    declarative_item=Or(
        A.simple_declarative_item,
        A.typed_string_decl,
        A.package_decl
    ),

    simple_declarative_items=List(
        Row(A.simple_declarative_item, ";")[0], empty_valid=True),

    simple_declarative_item=Or(
        A.variable_decl,
        A.attribute_decl,
        A.case_construction,
        A.empty_declaration
    ),

    variable_decl=Row(
        A.identifier,
        Opt(Row(":", A.static_name)[1]),
        ":=",
        A.expression
    ) ^ VariableDecl,

    attribute_decl=Row(
        "for", A.identifier,
        Opt(Row("(", A.associative_array_index, ")")[1]),
        "use",
        A.expression
    ) ^ AttributeDecl,

    associative_array_index=Or(
        A.others_designator,
        A.string_literal_at
    ),

    package_decl=Row(
        "package", A.identifier,
        Or(A.package_renaming, A.package_spec)
    ) ^ PackageDecl,

    package_renaming=Row(
        "renames", A.identifier, ".", A.identifier
    ) ^ PackageRenaming,

    package_extension=Row(
        "extends", A.identifier, ".", A.identifier,
    ) ^ PackageExtension,

    package_spec=Row(
        Opt(A.package_extension),
        "is",
        A.simple_declarative_items,
        "end", A.identifier
    ) ^ PackageSpec,

    empty_declaration=Row(
        "null"
    ) ^ EmptyDecl,

    case_construction=Row(
        "case", A.variable_reference, "is",
        List(A.case_item, empty_valid=True),
        "end", "case"
    ) ^ CaseConstruction,

    case_item=Row(
        "when",
        A.discrete_choice_list,
        "=>",
        # ??? A.declarative_items
        A.simple_declarative_items
    ) ^ CaseItem,

    others_designator=Tok("others") ^ OthersDesignator,
    choice=Or(A.string_literal, A.others_designator),

    discrete_choice_list=List(A.choice, sep="|"),

)
