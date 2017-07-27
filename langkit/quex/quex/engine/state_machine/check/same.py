from quex.engine.misc.tree_walker   import TreeWalker

def do(A, B): 
    """Check whether A and B match on same lexemes.

       RETURNS: True, if they do.
                False, else.
    """
    detector = SameDetector(A, B)
    detector.do((A.init_state_index, B.init_state_index))

    return detector.result

class SameDetector(TreeWalker):
    """Find acceptance states of 'A' which are reachable
       by walking along possible paths in 'B'. 
       
       Use the algorithm provided by 'Base_TunnelWalker' where
       "A = A" and "B = B".

       -- If an acceptance state in A ('A') is reached, then a pair
          (B_StateIndex, A_StateIndex) is appended to 'self.result'. 

       Later, Step2_Walker will walk along paths of 'B' starting 
       from these detected states to see whether it diverts.
    """
    def __init__(self, A, B):
        self.sm_a     = A # State Machine of the higher priority pattern
        self.sm_b     = B  # State Machine of the lower priority pattern
        self.result   = []
        self.done_set = set()
        TreeWalker.__init__(self)

    def on_enter(self, Args):
        # (*) Update the information about the 'trace of acceptances'
        A_StateIndex, B_StateIndex = Args
        if A_StateIndex in self.done_set: return None
        else:                             self.done_set.add(A_StateIndex)
        A_State = self.sm_a.states[A_StateIndex]
        B_State = self.sm_b.states[B_StateIndex]

        if A_State.is_acceptance() and B_State.is_acceptance():
            self.result  = True
            self.abort_f = True
            return None

        # Follow the path of common trigger sets
        sub_node_list = []
        for a_target, a_trigger_set in A_State.target_map.get_map().iteritems():
            for b_target, b_trigger_set in B_State.target_map.get_map().iteritems():
                if not b_trigger_set.has_intersection(a_trigger_set): continue
                # Some of the transition in 'A' is covered by a transition in 'B'.
                sub_node_list.append( (a_target, b_target) )

        return sub_node_list

    def on_finished(self, Args):
        pass
