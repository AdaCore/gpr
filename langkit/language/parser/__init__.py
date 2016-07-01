from langkit.compiled_types import abstract, ASTNode, root_grammar_class
from langkit.parsers import Grammar

gpr_grammar = Grammar(main_rule_name='compilation_unit')
A = gpr_grammar


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
