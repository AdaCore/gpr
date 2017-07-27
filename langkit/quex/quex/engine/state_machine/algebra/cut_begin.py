from   quex.engine.state_machine.core                 import StateMachine
from   quex.engine.state_machine.state.core           import State
import quex.engine.state_machine.algorithm.beautifier as     beautifier
import quex.engine.state_machine.index                as     index
from   quex.engine.misc.tree_walker                   import TreeWalker
from   quex.engine.misc.tools                              import r_enumerate

from   itertools import islice

def do(SM_A, SM_B):
    """Cut Begin:

    Let SM_A match the set of lexemes LA and SM_B match the set of lexemes LB.
    Then, the cut begin operation 'CutBegin'

                           SM_C = CutBegin(SM_A, SM_B)

    results in a state machine SM_C. The set of lexemes which it matches is
    given by 
                             .-
                             |   c(La) for all La in L(SM_A) where La
                             |         starts with one of L(SM_B).
                L(SM_C)  =  <          
                             |   La    for all other La from L(SM_A)
                             '-

    The cut operation 'c(La)' takes the elements Lb out of La that match SM_B.
    That is if La = [x0, x1, ... xi, xj, ... xN] and there is a Lb in L(SM_B)
    with Lb = [x0, x1, ... xi], then

                    c(La) = [xj, ... XN]
                           
    EXAMPLE 1: 

          NotBegin([0-9]+, [0-9]) = [0-9]{2,}

    That is where '[0-9]+' required at least one character in [0-9], the 
    cut version does not allow lexemes with one [0-9]. The result is a
    repetition of at least two characters in [0-9].

    EXAMPLE 2: 

          NotBegin(1(2?), 12) = 1

    Because the lexeme "12" is not to be matched by the result. The lexeme
    "1", though, does not start with "12". Thus, it remains.

    EXAMPLE 2: 

          NotBegin([a-z]+, print) = all identifiers except 'print'

    (C) 2013 Frank-Rene Schaefer
    """
    cutter = WalkAlong(SM_A, SM_B)
    cutter.do((SM_A.init_state_index, SM_B.init_state_index, None))

    # Delete orphaned and hopeless states in result
    cutter.result.clean_up()

    # Get propper state indices for result
    return beautifier.do(cutter.result)

class WalkAlong(TreeWalker):
    def __init__(self, SM_A, SM_B, result=None):
        self.original    = SM_A
        self.admissible  = SM_B

        if result is None:
            init_state_index = index.map_state_combination_to_index((SM_A.init_state_index, 
                                                                     SM_B.init_state_index))
            state            = self.get_state_core(SM_A.init_state_index)
            self.result      = StateMachine(InitStateIndex = init_state_index,
                                            InitState      = state)
        else:
            self.result      = result
        self.path        = []

        # Use 'operation_index' to get a unique index that allows to indicate
        # that 'SM_B' is no longer involved. Also, it ensures that the
        # generated state indices from (a_state_index, operation_index) are
        # unique.
        self.operation_index = index.get()

        TreeWalker.__init__(self)

    def on_enter(self, Args):
        a_state_index, b_state_index, trigger_set = Args
        assert b_state_index != self.operation_index

        if self.is_on_path(Args): 
            return None

        self.path.append((a_state_index, b_state_index, trigger_set))


        a_tm = self.original.states[a_state_index].target_map.get_map()
        if self.original.states[a_state_index].is_acceptance():
            # SM_A has reached a terminal
            if self.admissible.states[b_state_index].is_acceptance():
                # SM_B cuts the path until the terminal. 
                pass
            else:
                self.integrate_path_in_result()

        if len(a_tm) == 0:
            return None # No further path to walk along

        b_tm = self.admissible.states[b_state_index].target_map.get_map()
        #print "#loop:START", a_tm
        sub_node_list = []
        for a_ti, a_trigger_set in a_tm.iteritems():
            remainder = a_trigger_set.clone()
            #print "#a_trigger_set: %s" % a_trigger_set.get_utf8_string()
            for b_ti, b_trigger_set in b_tm.iteritems():
                intersection = a_trigger_set.intersection(b_trigger_set)
                if intersection.is_empty(): 
                    continue

                #print "#intersection:", intersection.get_utf8_string()
                sub_node_list.append((a_ti, b_ti, intersection))
                remainder.subtract(intersection)

            #print "#remainder: '%s'" % remainder.get_utf8_string()
            if not remainder.is_empty():
                #print "#B"
                # SM_B is not involved --> b_ti = self.operation_index
                self.path.append((a_ti, self.operation_index, remainder))
                #print "#result0:", self.result.get_string(NormalizeF=False)
                self.integrate_path_in_result()
                self.path.pop()
                #print "#result1:", self.result.get_string(NormalizeF=False)
                self.result.mount_cloned_subsequent_states(self.original, a_ti, self.operation_index)
                #print "#result2:", self.result.get_string(NormalizeF=False)
        #print "#loop:END", sub_node_list

        return sub_node_list

    def on_finished(self, Node):
        self.path.pop()

    def is_on_path(self, Args):
        a_state_index, b_state_index, dummy = Args
        for ai, bi, dummy in self.path:
            if ai == a_state_index and bi == b_state_index:
                return True
        return False

    def integrate_path_in_result(self):
        #print "#integrate_path_in_result:"
        #for i, x in enumerate(self.path):
        #    try:    #print "# [%i] %s, %s, %s" % (i, x[0], x[1], x[2].get_string(Option="utf8"))
        #    except: #print "# [%i] %s" % (i, x)

        for k, info in r_enumerate(self.path):
            dummy, bi, dummy = info
            if bi != self.operation_index and self.admissible.states[bi].is_acceptance():
                first_remainder_k = k + 1 # (ai, bi) is cut; next state is good
                break
        else:
            first_remainder_k = 1

        if first_remainder_k == len(self.path):
            # The last element of the path is an acceptance in SM_B, thus it is cut too.
            return # Nothing left.

        #print "#first_remainder_k:", first_remainder_k
        ai, bi, trigger_set = self.path[first_remainder_k]
        #print "#ai, bi:", ai, bi
        state_index, state  = self.get_state(ai, bi)
        if state_index != self.result.init_state_index:
            ##print "#(%s, %s) %s -- epsilon --> %s" % (ai, bi, self.result.init_state_index, state_index)
            self.result.get_init_state().target_map.add_transition(trigger_set, state_index)

        #print "#state.target_map:", state.target_map.get_map()
        #old_ti = state_index
        for ai, bi, trigger_set in islice(self.path, first_remainder_k+1, None):
            target_index, target_state = self.get_state(ai, bi)
            state.add_transition(trigger_set, target_index)
            #print "# %i -- %s --> %s" % (old_ti, trigger_set.get_utf8_string(), target_index)
            state = target_state
            #old_ti = target_index

        return
            
    def get_state_core(self, AStateIndex):
        acceptance_f = self.original.states[AStateIndex].is_acceptance() 
        return State(AcceptanceF=acceptance_f)

    def get_state(self, a_state_index, b_state_index):
        state_index = index.map_state_combination_to_index((a_state_index, b_state_index))
        state       = self.result.states.get(state_index)
        if state is None:
            state = self.get_state_core(a_state_index)
            self.result.states[state_index] = state
            #print "#enter:", state_index
        return state_index, state



