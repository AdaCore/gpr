import quex.engine.state_machine.construction.parallelize          as     parallelize
import quex.engine.state_machine.algorithm.beautifier as     beautifier

def do(SM_List):
    """The 'parallelize' module does a union of multiple state machines,
    even if they have different origins and need to be combined carefully.
    There is no reason, why another 'union' operation should be implemented
    in this case.
    """
    result = parallelize.do(SM_List)
    return beautifier.do(result)

