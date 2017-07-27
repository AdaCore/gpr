"""
"""
import quex.engine.misc.error as     error
from quex.engine.misc.file_in import read_until_letter, skip_whitespace, check_or_die, read_integer
from quex.blackboard          import setup as Setup

from bisect import bisect_left
from itertools import islice
from exception import EndOfStreamException

class StateStatistics:
    def __init__(self, ModeName, StateIndex, BoundaryList, CounterList):
        self.mode_name     = ModeName
        self.state_index   = StateIndex
        self.boundary_list = BoundaryList
        self.counter_list  = CounterList

        # The buffer limit code is not a character that has something to do
        # with the language, it is something that might appear in any state
        # as soon as a buffer limit is reached. 
        # => Exclude its occurence from any state-specific consideration
        self.blc   = Setup.buffer_limit_code
        self.blc_i = self.get_boundary_index(self.blc)
        self.counter_list[self.blc_i] = 0

    def get_outstanding_character(self, transition_list, AC):
        """An 'outstanding' character is considered to be a character that
           appears so exceptionally often that it is worth to implement a
           special test for it before the whole transition map. 
           
               if( input == outstanding_char ) {
                   goto ...;
               } else { 
                   ... remaining transition map ...
               }
           
           This means that for all other characters the cost increases by one
           comparison, but for the character itself the cost decreases down to
           a single comparison. 

                (1)  Cost0 = N * AC

           is the cost of normal bisectioning, given 'N' the number of character
           occurring and 'Ni' the number of intervals. Let 'No' be the number of
           time that the outstanding character occurred, then

                (2) Cost1 = (N - No) * (AC + 1)

           which is the cost for the remaining characters. And

                (3) Cost2 = No

           To actually gain something it is necessary ti required

                (4) Cost0 > Cost1 + Cost2

           thus,

                (5)        - N > No * (- (AC )

                (6)                N  
                            No > -----
                                   AC 

           Enequation (6) is now the requirement for an outstanding character.
           Note, that the average cost for classification 'AC' may vary
           dependent on how the transition map is implemented. If a switch-case
           construct is used most likely the transition cost is '1' because a
           'substract-indirect jump is implemented. For bisectioning the average
           cost would be 'log2(number of intervals)'.

           RETURNS: None - if there is no outstanding character.
                    int  - index of the outstanding character interval in 
                           transition_list.
        """
        assert AC != 0

        i_begin = self.get_boundary_index(transition_list[0][0].begin)
        
        # Determine total amount of appearances
        i_end = i_begin + len(transition_list)

        total_count = 0
        max_count   = -1
        max_i       = -1
        for i, count in enumerate(islice(self.counter_list, i_begin, i_end)):
            # The interval's size must be '1' to be a single character
            if transition_list[i].end > transition_list[i].begin + 1: continue
            total_count += count
            if count > max_count: max_count = count; max_i = i;
        
        if max_count <= (total_count / AC): return None
        else:                               return max_i

    def get_bisectioning_cost(self, transition_list):
        L = len(transition_list)
        if L == 0: return 0
        if L <= 2: return L - 1
        i = self.get_cut(transition_list)
        return   self.get_bisectioning_cost(transition_list[:i]) \
               + self.get_bisectioning_cost(transition_list[i:])

    def get_cut(self, transition_list):
        i_begin      = self.get_boundary_index(transition_list[0][0].begin)
        i_end        = len(transition_list) - 1
        total        = sum(islice(self.counter_list, i_begin, i_end))
        total_by_two = total / 2.0

        sum_count = 0
        for i, count in enumerate(islice(self.counter_list, i_begin, i_end)):
            sum_count += count
            if sum_count > total_by_two: break
        return i

    def get_boundary_index(self, Boundary):
        i_begin = bisect_left(self.boundary_list, Boundary)

        if Boundary != self.boundary_list[i_begin]: self.on_error()
        return i_begin

    def on_error(self):
        error.log("Statistics of state %i in mode %s do not fit analyzer structure." \
                  % (self.state_index, self.mode_name))

def do(Filename):
    fh = open(Filename, "rb")

    db = {}
    try:
        while 1 + 1 == 2:
            state_index, statistics = parse_state_statistics(fh)
            db[state_index] = statistics
    except EndOfStreamException as x:
        x
        return db
    
def parse_state_statistics(fh):
    skip_whitespace(fh)
    check_or_die(fh, "{")

    check_or_die(fh, "mode:")
    skip_whitespace(fh)
    mode_name = read_until_letter(fh, ";")

    check_or_die(fh, "state:")
    skip_whitespace(fh)
    state_index = read_integer(fh)
    check_or_die(fh, ";")

    check_or_die(fh, "{")
    skip_whitespace(fh)
    boundary_list_str = read_until_letter(fh, ";")
    skip_whitespace(fh)
    counter_list_str  = read_until_letter(fh, ";")
    skip_whitespace(fh)
    check_or_die(fh, "}")
    check_or_die(fh, "}")

    def help(X): map(lambda x: int(x), X.strip().split())

    return StateStatistics(mode_name, state_index, 
                           help(boundary_list_str), 
                           help(counter_list_str))
