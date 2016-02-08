from langkit.compiled_types import abstract, ASTNode, root_grammar_class
from langkit.parsers import Grammar

gpr_grammar = Grammar()
A = gpr_grammar
gpr_grammar.main_rule_name = "project"


@abstract
@root_grammar_class
class GPRNode(ASTNode):
    pass


def eval_grammar():
    from language.parser import A

    import language.parser.decl
    import language.parser.types
    import language.parser.exprs
    import language.parser.bodies

    del language
    return A

ada_grammar = eval_grammar()
