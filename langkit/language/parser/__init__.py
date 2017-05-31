from langkit.dsl import abstract, ASTNode
from langkit.parsers import Grammar

gpr_grammar = Grammar(main_rule_name='compilation_unit')
A = gpr_grammar


@abstract
class GPRNode(ASTNode):
    _generic_list_type = 'BaseList'


def eval_grammar():
    from language.parser import A

    import language.parser.decl
    import language.parser.types
    import language.parser.exprs
    import language.parser.bodies

    del language
    return A


ada_grammar = eval_grammar()
