from quex.engine.misc.tools import flatten_list_of_lists
from quex.blackboard   import E_IncidenceIDs, E_TransitionN

from operator        import attrgetter
from collections     import namedtuple, defaultdict
from itertools       import islice, izip
#______________________________________________________________________________
# Result of the analysis: For each state there is a list of AcceptSequences
# each one representing a path through the state machine to that state. An 
# AcceptSequences is a list of AcceptCondition objects.
# 
AcceptCondition = namedtuple("AcceptCondition", 
                             ("acceptance_id", 
                              "pre_context_id", 
                              "accepting_state_index", 
                              "positioning_state_index",
                              "transition_n_since_positioning"))

class AcceptSequence:
    def __init__(self, AcceptanceTrace):
        self.__sequence = [
           AcceptCondition(x.acceptance_id, 
                           x.pre_context_id, 
                           x.accepting_state_index, 
                           x.positioning_state_index, 
                           x.transition_n_since_positioning)
           for x in AcceptanceTrace
        ]

    def acceptance_behavior_equal(self, Other):
        if len(self.__sequence) != len(Other.__sequence):    
            return False
        for x, y in izip(self.__sequence, Other.__sequence):
            if   x.pre_context_id != y.pre_context_id: return False
            elif x.acceptance_id  != y.acceptance_id:  return False
        return True

    def __iter__(self):
        return self.__sequence.__iter__()

    def get_string(self, Indent=0):
        txt = [ " " * (Indent*4) + "p-id           pre-id   as-i     ps-i     tnsp\n"]
        for x in self.__sequence:
            #012345678012345678012345678012345678012345678
            txt.append(" " * (Indent*4) + "%-15s%-9s%-9s%-9s%-9s\n" % ( \
                        x.acceptance_id, x.pre_context_id,
                        x.accepting_state_index, x.positioning_state_index,
                        x.transition_n_since_positioning))
        return "".join(txt)

    def __str__(self):
        return self.get_string()

class PositioningInfo(object):
    __slots__ = ("pre_context_id", 
                 "acceptance_id",
                 "transition_n_since_positioning", 
                 "positioning_state_index_set")
    def __init__(self, TheAcceptCondition):
        self.pre_context_id                 = TheAcceptCondition.pre_context_id
        self.acceptance_id                     = TheAcceptCondition.acceptance_id
        self.transition_n_since_positioning = TheAcceptCondition.transition_n_since_positioning
        self.positioning_state_index_set    = set([ TheAcceptCondition.positioning_state_index ])

    def add(self, TheAcceptCondition):
        self.positioning_state_index_set.add(TheAcceptCondition.positioning_state_index)

        if self.transition_n_since_positioning != TheAcceptCondition.transition_n_since_positioning:
            self.transition_n_since_positioning = E_TransitionN.VOID

    def __repr__(self):
        txt  = ".acceptance_id                     = %s\n" % repr(self.acceptance_id) 
        txt += ".pre_context_id                 = %s\n" % repr(self.pre_context_id) 
        txt += ".transition_n_since_positioning = %s\n" % repr(self.transition_n_since_positioning)
        txt += ".positioning_state_index_set    = %s\n" % repr(self.positioning_state_index_set) 
        return txt

class PathsToState:
    def __init__(self, TraceList):
        self.__list = [ 
            AcceptSequence(x.acceptance_trace) for x in TraceList 
        ]

        # (*) Uniform Acceptance Sequence
        #
        #         map: state_index --> acceptance pattern
        #
        #     If all paths to a state show the same acceptance pattern, than this
        #     pattern is stored. Otherwise, the state index is related to None.
        self.__uniform_acceptance_sequence  = -1 # Undone

        # (*) Acceptance Precedence Matrix
        # 
        #     A matrix that can tell for any two pattern ids A and B:
        #
        #         -- A has always precedence over B
        #         -- B has always precedence over A
        #         -- A and B never occur together in a path.
        #
        #     If there are any two patterns where none of the above 
        #     conditions holds it is considered a 'acceptance precedence clash'.
        self.__acceptance_precedence_matrix = -1 # Undone

        # (*) Set of pattern ids occurring here
        self.__pattern_id_list               = -1 # Undone

        # (*) Positioning info:
        #
        #     map:  (state_index) --> (acceptance_id) --> positioning info
        #
        self.__positioning_info             = -1 # Undone

    def get_any_one(self):
        assert len(self.__list) != 0
        return self.__list[0]

    def uniform_acceptance_sequence(self):
        """
        This function draws conclusions on the input acceptance behavior at
        drop-out based on different paths through the same state. Basis for
        the analysis are the PathTrace objects of a state specified as
        'ThePathTraceList'.

        Acceptance Uniformity:

            For any possible path to 'this' state the acceptance pattern is
            the same. That is, it accepts exactly the same pattern under the
            same pre contexts and in the same sequence of precedence.

        The very nice thing is that the 'acceptance_trace' of a PathTrace
        object reflects the precedence of acceptance. Thus, one can simply
        compare the acceptance trace objects of each PathTrace.

        RETURNS: list of AcceptInfo() - uniform acceptance pattern.
                 None                 - acceptance pattern is not uniform.
        """
        if self.__uniform_acceptance_sequence != -1:
            return self.__uniform_acceptance_sequence

        prototype = self.__list[0]

        # Check (1) and (2)
        for accept_sequence in islice(self.__list, 1, None):
            if not accept_sequence.acceptance_behavior_equal(prototype): 
                self.__uniform_acceptance_sequence = None
                break
        else:
            self.__uniform_acceptance_sequence = prototype

        return self.__uniform_acceptance_sequence

    def accepting_state_index_list(self):
        return flatten_list_of_lists(
            (x.accepting_state_index for x in acceptance_sequence)
            for acceptance_sequence in self.__list
        )

    def acceptance_precedence_clash(self):
        # self.__acceptance_precedence_matrix_contruct() is None
        # => there was a clash in the precedence matrix
        return self.acceptance_precedence_matrix() is None

    def acceptance_precedence_get(self, AcceptanceID_A, AcceptanceID_B):
        """RETURNS:
              1   => AcceptanceID_A has always precedence over AcceptanceID_B

              0   => AcceptanceID_A and AcceptanceID_B have always same precedence,
                     only possible of both are the same.

             -1   => AcceptanceID_B has always precedence over AcceptanceID_A

             None => AcceptanceID_A and AcceptanceID_A never occur together.
        """
        # This function should never be called, if there was a acceptance
        # precedence clash.
        assert not self.acceptance_precedence_clash()

        matrix = self.acceptance_precedence_matrix()

        if AcceptanceID_A == AcceptanceID_B:
            return 0

        if AcceptanceID_A > AcceptanceID_B:
            return matrix[AcceptanceID_A][AcceptanceID_B]

        result = matrix[AcceptanceID_B][AcceptanceID_A]
        if result is None:
            return None
        return - result

    def acceptance_precedence_matrix(self):
        """IMPORTANT: The matrix contains only entries [x][y] for x < y.
                      This means, that queries into the matrix need to be 
                      sorted and inverted, if necessary. 

           Consider: acceptance_precedence_get() !
        """
        if self.__acceptance_precedence_matrix == -1:
            self.__acceptance_precedence_matrix = self.__acceptance_precedence_matrix_contruct()

        return self.__acceptance_precedence_matrix

    def pattern_id_list(self):
        if self.__pattern_id_list == -1:
            result = set()
            for accept_sequence in self.__list:
                result.update(x.acceptance_id for x in accept_sequence)
            result.sort(key=attrgetter("acceptance_id"))
            self.__pattern_id_list = result
        return self.__pattern_id_list

    def positioning_info(self):
        """
        Conclusions on the input positioning behavior at drop-out based on
        different paths through the same state.  Basis for the analysis are the
        PathTrace objects of a state specified as 'ThePathTraceList'.

        RETURNS: For a given state's PathTrace list a dictionary that maps:

                            acceptance_id --> PositioningInfo

        --------------------------------------------------------------------
        
        There are the following alternatives for setting the input position:
        
           (1) 'lexeme_start_p + 1' in case of failure.

           (2) 'input_p + offset' if the number of transitions between
               any storing state and the current state is does not differ 
               dependent on the path taken (and does not contain loops).
        
           (3) 'input_p = position_register[i]' if (1) and (2) are not
               not the case.

        The detection of loops has been accomplished during the construction
        of the PathTrace objects for each state. This function focusses on
        the possibility to have different paths to the same state with
        different positioning behaviors.
        """
        if self.__positioning_info != -1: 
            return self.__positioning_info

        positioning_info_by_pattern_id = {}
        # -- If the positioning differs for one element in the trace list, or 
        # -- one element has undetermined positioning, 
        # => then the acceptance relates to undetermined positioning.
        for acceptance_sequence in self.__list:
            for x in acceptance_sequence:
                assert x.acceptance_id != E_IncidenceIDs.VOID

                prototype = positioning_info_by_pattern_id.get(x.acceptance_id)
                if prototype is None:
                    positioning_info_by_pattern_id[x.acceptance_id] = PositioningInfo(x)
                else:
                    prototype.add(x)

        self.__positioning_info = positioning_info_by_pattern_id.values()
        return self.__positioning_info

    def __acceptance_precedence_matrix_contruct(self):
        def matrix_get(matrix, Xid, Yid):
            a = matrix.get(Xid)
            if len(a) == 0: return None
            b = a.get(Yid)
            if b is None: return None
            return b

        # pattern_id_list is sorted by acceptance_id
        pattern_id_list = self.pattern_id_list()
        result = defaultdict(dict)
        for i, x_pattern_id in enumerate(pattern_id_list):
            for y_pattern_id in pattern_id_list[i+1:]:
                for accept_sequence in self.__list:
                    value          = accept_sequence.get_precedence(x_pattern_id, y_pattern_id)
                    existing_value = matrix_get(result, x_pattern_id, y_pattern_id)
                    if   existing_value is None:
                        result[x_pattern_id][y_pattern_id] = value 
                    elif existing_value != value:
                        return None
                    else:
                        pass # nothing to be done
        return result

    def __iter__(self):
        return self.__list.__iter__()

