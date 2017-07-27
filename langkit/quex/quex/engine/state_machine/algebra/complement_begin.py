import quex.engine.state_machine.algorithm.beautifier as     beautifier
import quex.engine.state_machine.check.special        as     special
import quex.engine.state_machine.index                as     index
from   quex.engine.state_machine.core                 import StateMachine
from   quex.engine.state_machine.state.core           import State
from   quex.engine.misc.tree_walker                   import TreeWalker


def do(SM_A, SM_B):
    """Complement Begin:

    Let SM_A match the set of lexemes LA and SM_B match the set of lexemes LB.
    Then, the complement begin operation 'NotBegin'

                           SM_C = NotBegin(SM_A, SM_B)

    results in a state machine SM_C, matches all lexemes of LA except for those
    that start with a lexeme from LB.

    EXAMPLE 1: 

          NotBegin([0-9]+, [0-9]) = \None

    EXAMPLE 2: 

          NotBegin(1(2?), 12) = 1

    Because the lexeme "12" is not to be matched by the result. The lexeme
    "1", though, does not start with "12". Thus, it remains.

    EXAMPLE 2: 

          NotBegin([a-z]+, print) = all identifiers except 'print'

    (C) 2013 Frank-Rene Schaefer
    """
    cutter = WalkAlong(SM_A, SM_B)
    if SM_B.get_init_state().is_acceptance():
        return special.get_none()

    cutter.do((SM_A.init_state_index, SM_B.init_state_index))

    # Delete orphaned and hopeless states in result
    cutter.result.clean_up()

    # Get propper state indices for result
    return beautifier.do(cutter.result)

class WalkAlong(TreeWalker):
    def __init__(self, SM_A, SM_B, StartingSM=None):
        self.original   = SM_A
        self.admissible = SM_B

        if StartingSM is None:
            self.result = StateMachine(InitStateIndex = index.map_state_combination_to_index((SM_A.init_state_index, 
                                                                                              SM_B.init_state_index)), 
                                       InitState      = self.get_state_core(SM_A.init_state_index, 
                                                                            SM_B.init_state_index))
        else:
            self.result = StartingSM

        # TODO: Think if 'state_db' cannot be replaced by 'result'
        self.state_db   = {}

        self.path       = []

        # Use 'operation_index' to get a unique index that allows to indicate
        # that 'SM_B' is no longer involved. Also, it ensures that the
        # generated state indices from (a_state_index, operation_index) are
        # unique.
        self.operation_index = index.get()

        TreeWalker.__init__(self)

    def on_enter(self, Args):
        if Args in self.path: 
            return None

        a_state_index, b_state_index = Args
        self.path.append((a_state_index, b_state_index))

        state = self.get_state(Args)

        sub_node_list = []

        a_tm = self.original.states[a_state_index].target_map.get_map()
        assert b_state_index != self.operation_index

        b_tm = self.admissible.states[b_state_index].target_map.get_map()
        for a_ti, a_trigger_set in a_tm.iteritems():
            remainder = a_trigger_set.clone()
            for b_ti, b_trigger_set in b_tm.iteritems():
                # If an acceptance state in 'B' is reached, than the lexeme starts
                # with something in 'LB'. Thus, rest of paths is inadmissible.
                if self.admissible.states[b_ti].is_acceptance(): 
                    remainder.subtract(b_trigger_set)
                    continue                                     

                intersection = a_trigger_set.intersection(b_trigger_set)
                if intersection.is_empty(): 
                    continue

                combi = (a_ti, b_ti)
                state.add_transition(intersection, index.map_state_combination_to_index(combi))
                sub_node_list.append(combi)

                remainder.subtract(intersection)

            if not remainder.is_empty():
                combi = (a_ti, self.operation_index)
                state.add_transition(remainder, index.map_state_combination_to_index(combi))
                self.result.mount_cloned_subsequent_states(self.original, a_ti, self.operation_index)

        ## print "#1-sub_node_list:", sub_node_list
        return sub_node_list

    def on_finished(self, Node):
        self.path.pop()

    def get_state_core(self, AStateIndex, BStateIndex):
        acceptance_f = self.original.states[AStateIndex].is_acceptance() 
        return State(AcceptanceF=acceptance_f)

    def get_state(self, Args):
        state_index = index.map_state_combination_to_index(Args)
        state       = self.state_db.get(state_index)
        if state is None:
            a_state_index, b_state_index = Args
            state = self.get_state_core(a_state_index, b_state_index)
            self.result.states[state_index] = state
        return state

