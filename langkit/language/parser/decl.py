from language.parser import A
from langkit.parsers import Opt, List, Or, Pick
from langkit.dsl import Field

from language.parser import GPRNode


class AllQualifier(GPRNode):
    enum_node = True
    qualifier = True


class Limited(GPRNode):
    enum_node = True
    qualifier = True


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


class Choices(GPRNode.list):
    pass


A.add_rules(
    context_clauses=List(A.with_decl, empty_valid=True),

    with_decl=WithDecl(
        Opt("limited").as_bool(Limited),
        "with", List(A.string_literal, sep=","), ";"
    ),

    abstract_present=AbstractPresent(
        "abstract"
    ),

    qualifier_names=QualifierNames(
        A.identifier, Opt(A.identifier)
    ),

    project_qualifier=ProjectQualifier(Or(
        A.abstract_present,
        A.qualifier_names
    )),

    project_extension=ProjectExtension(
        "extends", Opt("all").as_bool(AllQualifier), A.string_literal
    ),

    project_declaration=ProjectDeclaration(
        Opt(A.project_qualifier),
        "project",
        A.static_name,
        Opt(A.project_extension),
        "is",
        A.declarative_items,
        "end", A.static_name, ";"
    ),

    project=Project(
        A.context_clauses,
        A.project_declaration,
    ),

    # ----------------------------------------------- declarative items

    declarative_items=List(A.declarative_item, empty_valid=True),

    declarative_item=Or(
        A.simple_declarative_item,
        A.typed_string_decl,
        A.package_decl
    ),

    simple_declarative_items=List(A.simple_declarative_item, empty_valid=True),

    simple_declarative_item=Or(
        A.variable_decl,
        A.attribute_decl,
        A.case_construction,
        A.empty_declaration
    ),

    variable_decl=VariableDecl(
        A.identifier,
        Opt(Pick(":", A.type_reference)),
        ":=",
        A.expression, ";"
    ),

    attribute_decl=AttributeDecl(
        "for", A.identifier,
        Opt(Pick("(", A.associative_array_index, ")")),
        "use",
        A.expression, ";"
    ),

    associative_array_index=Or(
        A.others_designator,
        A.string_literal_at
    ),

    package_decl=PackageDecl(
        "package", A.identifier,
        Or(A.package_renaming, A.package_spec), ";"
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
        "end", A.identifier
    ),

    empty_declaration=EmptyDecl("null", ";"),

    case_construction=CaseConstruction(
        "case", A.variable_reference, "is",
        List(A.case_item, empty_valid=True),
        "end", "case", ";"
    ),

    case_item=CaseItem(
        "when",
        A.discrete_choice_list,
        "=>",
        # ??? A.declarative_items
        A.simple_declarative_items
    ),

    others_designator=OthersDesignator("others"),
    choice=Or(A.string_literal, A.others_designator),

    discrete_choice_list=List(A.choice, sep="|", list_cls=Choices),

)
