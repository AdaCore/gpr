from   quex.output.core.state.transition_map.bisection           import Bisection
from   quex.output.core.state.transition_map.branch_table        import BranchTable
from   quex.output.core.state.transition_map.comparison_sequence import ComparisonSequence
from   quex.engine.analyzer.transition_map                            import TransitionMap  

from   quex.engine.misc.enum import Enum

E_Solution = Enum("COMPARISON_SEQUENCE", "BRANCH_TABLE", "BISECTIONING", "__DEBUG_Solution")

def do(TM):
    return get_structure(TM)

def get_solution(TM):
    """RETURNS: [0] Solution from E_Solution
                [1] Most often appearing target
    """
    interval_n = len(TM)
    assert interval_n > 0

    most_often_appearing_target, target_n = TransitionMap.get_target_statistics(TM)

    # If there's only one interval, there's no need to compare, just go!
    # Otherwise, if there's a very low number of intervals, make a small
    # comparison list that iterates linearly through the items.
    if target_n < 4 and interval_n < 6: 
        return E_Solution.COMPARISON_SEQUENCE, None

    # If the size of character ranges which do not target 'moat' is less
    # than a certain number, implement the transition as branch table. The
    # 'moat' is implemented as the 'default:' case.
    sz_non_moat = TransitionMap.get_size_of_range_other_targets(TM, most_often_appearing_target)
    if sz_non_moat < 256: 
        return E_Solution.BRANCH_TABLE, most_often_appearing_target

    return E_Solution.BISECTIONING, None

def get_structure(TM): 
    """__dive --> indicate recursion that might be replaced by TreeWalker
    """
    solution, moat = get_solution(TM)

    if   solution == E_Solution.COMPARISON_SEQUENCE: return ComparisonSequence(TM)
    elif solution == E_Solution.BRANCH_TABLE:        return BranchTable(TM, moat)

    # Else, there is nothing left but bisectioning
    # (which is not the worst thing to do)
    return get_Bisection(TM)

def get_Bisection(TM):
    """BranchTables and Comparison sequences are considered to be 'better'
    than bisectioning. Thus, this function tries to set the bisectioning value
    so that the two parts are both feasible by either BranchTable or 
    ComparisonSequence. That is, if the bisectioning into

                                     N = L / 2
             |-----------------------|-----------------------|
                   bisectioning         comparison sequence

    can be replaced by 
                                 Q
             |-------------------|---------------------------|
                   branch table            branch table

    then, the bisectioning is better done at Q rather than N.
    """
    L = len(TM) / 2
    assert L >= 1

    tm0 = TM[:L]
    tm1 = TM[L:]
    bisection_value = tm0[-1][0].end
    low  = get_structure(tm0)
    high = get_structure(tm1)
    return Bisection(bisection_value, low, high)

