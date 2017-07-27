import quex.output.core.state.transition_map.transition as transition
from   quex.engine.analyzer.transition_map import TransitionMap
from   quex.blackboard import setup as Setup, \
                              Lng

class ComparisonSequence(object):
    __slots__ = ("sub_map")
    def __init__(self, SubMap):
        self.sub_map = SubMap

    def implement(self):
        L = len(self.sub_map)
        assert L != 0

        tm = [
            (interval, "".join(transition.do(interval, target)))
            for interval, target in self.sub_map
        ]

        if len(tm) == 1:
            return Lng.COMPARISON_SEQUENCE(tm, None)

        tm, default = ComparisonSequence.optimize(tm)

        # The buffer limit code is appears extreme seldomly
        # => if it's there, make sure that it is tested at last. 
        #    (This might require to reverse the trigger map.)
        # The 'BLC' might actually no longer occur in the optimized map. Thus, 
        # search for it in the original transition map.
        blc_index = TransitionMap.bisect(self.sub_map, Setup.buffer_limit_code)
        if blc_index is not None and blc_index < L / 2:
            def get_decision(interval, i, L):
                if   i == L-1:             return Lng.ELSE_SIMPLE
                elif interval.size() == 1: return Lng.IF_X("==", interval.begin, i, L)
                else:                      return Lng.IF_X(">=", interval.begin, i, L)

            tm = list(reversed(tm))
        else:
            def get_decision(interval, i, L):
                if   i == L-1:             return Lng.ELSE_SIMPLE
                elif interval.size() == 1: return Lng.IF_X("==", interval.begin, i, L)
                else:                      return Lng.IF_X("<",  interval.end,   i, L)

        if default is not None: tm.append(default)
        return Lng.COMPARISON_SEQUENCE(tm, get_decision)

    @staticmethod
    def optimize(tm):
        """Special case: a sequence of intervals where

            (a) every second interval has the same effect and
            (b) every interval in between is of size '1'.

        In that case, the comparison sequence can be simplified to checks of
        the intervals in between + a single else statement that catches all
        the intervals have the same effect.

        The 'even' sequence (0, 2, 4 ...) is ALWAYS either equally efficient or 
        better than the 'odd' sequence (1, 3, ...). Thus, prefer the even 
        sequence in any case. (May be write down an example).

        TRICK: If the every second interval was deleted, then add a 'dummy
               interval at the end! It will be implemented as 'ELSE_SIMPLE'
               anyway!

        RETURNS: [0] The (optimized) transition map
                 [1] The 'default' case.

        In case, no optimization happened, the 'default' case is None.
        """
        def detect_a(tm, Index):
            """Detect whether every second interval has the same effect 
            starting with 'Index'.
            """
            previous = None
            for target in (tm[i][1] for i in xrange(Index, len(tm), 2)):
                if   previous is None:   previous = target
                elif previous != target: return False
            return True

        def detect_b(tm, Index):
            """Detect whether every second interval is of size '1'.
            """
            for interval in (tm[i][0] for i in xrange(Index, len(tm), 2)):
                if interval.size() != 1: return False
            return True

        # First, check for the even sequence since it is preferable.
        if detect_a(tm, 0) and detect_b(tm, 1):
            # Delete the even intervals
            return [ tm[i] for i in xrange(1, len(tm), 2) ], \
                   (None, tm[0][1])  # The 'else' interval

        # Second, check the odd sequence.
        elif detect_a(tm, 1) and detect_b(tm, 0):
            return [ tm[i] for i in xrange(0, len(tm), 2) ], \
                   (None, tm[1][1])   # The 'else' interval

        # No optimization possible
        else:
            return tm, None


