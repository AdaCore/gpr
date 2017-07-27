"""
ABSTRACT:

    The UTF8-State-Split is a procedure introcuded by Frank-Rene Schaefer that
    allows to transform a state machine that triggers on unicode characters
    into a state machine that triggers on the correspondent UTF8 Byte
    Sequences.

PRINCIPLE:

    An elementary trigger in quex state machine is a unicode interval. That
    means, that if a character appears that falls into the interval a state
    transition is triggered. Each of those intervals needs now to be translated
    into interval sequences of the correspondent utf8 byte sequences. A unicode
    transition from state A to state B:

         [ A ]-->(x0, x1)-->[ B ]

    is translated into a chain of utf8-byte sequence transitions that might
    look like this

         [ A ]-->(b0)-->[ 1 ]-->(c0,c1)-->[ B ] 
             \                             /
              `->(d1)-->[ 2 ]---(e0,e1)---' 

    That means that intermediate states may be introduced to reflect the
    different byte sequences that represent the original interval.

IDEAS:
    
    In a simple approach one would translate each element of a interval into an
    utf8-byte sequence and generate state transitions between A and B.  Such an
    approach, however, produces a huge computational overhead and charges the
    later Hopcroft Minimization with a huge state machine.

    To avoid such an overflow, the Hopcroft Minimzation can be prepared on the
    basis of transition intervals. 
    
    (A) Backwards: In somewhat greater intervals, the following might occur:


                 .-->(d1)-->[ 1 ]---(A3,BF)---. 
                /                              \
               /  ,->(d1)-->[ 2 ]---(80,BF)--.  \
              /  /                            \  \
             [ A ]-->(b0)-->[ 3 ]-->(80,BF)-->[ B ] 
                 \                             /
                  `->(d1)-->[ 4 ]---(80,81)---' 

        That means, that for states 2 and 3 the last transition is on [80, BF]
        to state B. Thus, the intermediate states 2 and 3 are equivalent. Both
        can be replaced by a single state. 

    (B) Forwards: The first couple of bytes in the correspondent utf8 sequences
        might be the same. Then, no branch is required until the first differing
        byte.

PROCESS:

    (1) The original interval is split into sub-intervals that have the same 
        length of utf8-byte sequences.

    (2) Each sub-interval is split into further sub-intervals where as 
        many trailing [80,BF] ranges are combined.

    (3) The interval sequences are plugged in between the state A and B
        of the state machine.

(C) 2009 Frank-Rene Schaefer
"""
import os
import sys
from   copy import copy
sys.path.append(os.environ["QUEX_PATH"])

from   quex.engine.misc.utf8                     import utf8_to_unicode, unicode_to_utf8, UTF8_MAX, UTF8_BORDERS
from   quex.engine.misc.interval_handling        import Interval, NumberSet
import quex.engine.state_machine            as     state_machine
from   quex.engine.state_machine.state.core import State
import quex.engine.state_machine.algorithm.beautifier as beautifier

utf8_border = [ 0x00000080, 0x00000800, 0x00010000, 0x00110000] 

def do(sm):
    """The UTF8 encoding causes a single unicode character code being translated
       into a sequence of bytes. A state machine triggering on unicode characters
       can be converted into a state machine triggering on UTF8 bytes.

       For this a simple transition on a character 'X':

            [ 1 ]---( X )--->[ 2 ]

       needs to be translated into a sequence of state transitions

            [ 1 ]---(x0)--->[ S0 ]---(x1)--->[ S1 ]---(x2)--->[ 2 ]

       where, x0, x1, x2 are the UTF8 bytes that represent unicode 'X'. 
       States S0 and S1 are intermediate states created only so that
       x1, x2, and x3 can trigger. Note, that the UTF8 sequence ends
       at the same state '2' as the previous single trigger 'X'.
    """
    state_list = sm.states.items()
    for state_index, state in state_list:
        # Get the 'transition_list', i.e. a list of pairs (TargetState, NumberSet)
        # which indicates what target state is reached via what number set.
        transition_list = state.target_map.get_map().items()
        # Clear the state's transitions, now. This way it can absorb new
        # transitions to intermediate states.
        state.target_map.clear()
        # Loop over all transitions
        for target_state_index, number_set in transition_list:
            # We take the intervals with 'PromiseToTreatWellF' even though they
            # are changed. This is because the intervals would be lost anyway
            # after the state split, so we use the same memory and do not 
            # cause a time consuming memory copy and constructor calls.
            for interval in number_set.get_intervals(PromiseToTreatWellF=True):
                create_intermediate_states(sm, state_index, target_state_index, interval)

    return beautifier.do(sm)

def do_set(NSet):
    """Unicode values > 0x7F are translated into byte sequences, thus, only number
       sets below that value can be transformed into number sets. They, actually
       remain the same.
    """
    for interval in NSet.get_intervals(PromiseToTreatWellF=True):
        if interval.end > 0x80: return None
    return NSet

def homogeneous_chunk_n_per_character(CharacterSet):
    """If all characters in a unicode character set state machine require the
    same number of bytes to be represented this number is returned.  Otherwise,
    'None' is returned.

    RETURNS:   N > 0  number of bytes required to represent any character in the 
                      given state machine.
               None   characters in the state machine require different numbers of
                      bytes.
    """
    assert isinstance(CharacterSet, NumberSet)

    interval_list = CharacterSet.get_intervals(PromiseToTreatWellF=True)
    front = interval_list[0].begin     # First element of number set
    back  = interval_list[-1].end - 1  # Last element of number set
    # Determine number of bytes required to represent the first and the 
    # last character of the number set. The number of bytes per character
    # increases monotonously, so only borders have to be considered.
    front_chunk_n = len(unicode_to_utf8(front))
    back_chunk_n  = len(unicode_to_utf8(back))
    if front_chunk_n != back_chunk_n: return None
    else:                             return front_chunk_n

def create_intermediate_states(sm, state_index, target_state_index, X):
    db = split_interval_according_to_utf8_byte_sequence_length(X)
    if db is None:
        return

    # Split interval into sub intervals where the utf8-sequence has
    # the same number of bytes
    for seq_length, interval in db.items():
        e_list, first_diff_byte_idx = split_interval_into_contigous_byte_sequence_range(interval, seq_length)
        trigger_set_sequence_db = \
           map(lambda x: get_trigger_sequence_for_contigous_byte_range_interval(x, seq_length),
               e_list)

        plug_state_sequence_for_trigger_set_sequence(sm, state_index, target_state_index,
                                                     trigger_set_sequence_db, seq_length, 
                                                     first_diff_byte_idx) 

def split_interval_according_to_utf8_byte_sequence_length(X):
    """Split Unicode interval into intervals where all values
       have the same utf8-byte sequence length.
    """
    if X.begin < 0:         X.begin = 0
    if X.end   > UTF8_MAX:  X.end   = UTF8_MAX + 1

    if X.size() == 0: return None

    db = {}
    current_begin = X.begin
    last_L        = len(unicode_to_utf8(X.end - 1))  # Length of utf8 sequence corresponding
    #                                                # the last value inside the interval.
    while 1 + 1 == 2:
        L = len(unicode_to_utf8(current_begin))   # Length of the first unicode in utf8
        # Store the interval together with the required byte sequence length (as key)
        current_end = UTF8_BORDERS[L-1]
        if L == last_L: 
            db[L] = Interval(current_begin, X.end)
            break
        db[L] = Interval(current_begin, current_end)
        current_begin = current_end

    return db
    
def split_interval_into_contigous_byte_sequence_range(X, L):
    """Use the fact that utf8 byte sequences of increasing unicode values relate
       to increasing byte sequence values. Consider the unicode interval [0x12345,
       0x17653]. 
       
                    Unicode   UTF8-byte sequence

                    012345    F0.92.8D.85
                              ...
                    01237F    F0.92.8D.BF
                    012380    F0.92.8E.80
                              ...
                    012FFF    F0.92.BF.BF
                    013000    F0.93.80.80
                              ...
                    016FFF    F0.96.BF.BF
                    017000    F0.97.80.80
                              ...
                    01763F    F0.97.98.BF
                    017640    F0.97.99.80
                              ...
                    017653    F0.97.99.93

       
       The utf8 sequences of the values in the sub-interval [0x12345, 0x1237F]
       only differ with respect to the last byte, but they all trigger to the
       'original targte state', so they can be combined into a trigger sequence

                 [F0, 92, 8D, [85,BF]]

       Analogously, the values in [0x12FFF, 0x13000] differ only with respect
       to the last two bytes. But, all trigger with 2x [80, BF] to the original
       target state. So, they can be combined to the original target state, thus
       they can be combined to

                 [F0, 92, [80,BF], [80,BF]]

       A contigous interval is an interval where such combinations are valid.
       This function splits a given interval into such intervals.

       REQUIRES: The byte sequence in the given interval **must** have all the same 
                 length L.

       RETURNS: List of 'contigous' intervals and the index of the first byte
                where all sequences differ.
    """
    # A byte in a utf8 sequence can only have a certain range depending
    # on its position. UTF8 sequences look like the following dependent
    # on their length:
    #
    #       Length:   Byte Masks for each byte
    #
    #       1 byte    0xxxxxxx
    #       2 bytes   110xxxxx 10xxxxxx
    #       3 bytes   1110xxxx 10xxxxxx 10xxxxxx
    #       4 bytes   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    #       5 bytes   ...
    #
    # where 'free' bits are indicated by 'x'. 
    # Min. value of a byte = where all 'x' are zero.
    # Max. value of a byte = where all 'x' are 1.
    # 
    def min_byte_value(ByteIndex):
        assert L <= 6
        if ByteIndex != 0: return 0x80
        # Only first byte's range depends on length
        return { 0: 0x00, 1: 0xC0, 2: 0xE0, 3: 0xF0, 4: 0xF8, 5: 0xFC }[L]

    def max_byte_value(ByteIndex):
        assert L <= 6
        if ByteIndex != 0: return 0xBF
        # Only first byte's range depends on length
        return { 0: 0x7F, 1: 0xDF, 2: 0xEF, 3: 0xF7, 4: 0xFB, 5: 0xFD }[L]
       
    def find_first_diff_byte(front_sequence, back_sequence):
        # Find the first byte that is different in the front and back sequence 
        for i in range(L-1):
            if front_sequence[i] != back_sequence[i]: return i
        # At least the last byte must be different. That's why it **must** be the
        # one different if no previous byte was it.
        return L - 1

    assert X.size() != 0

    if X.size() == 1: return [ X ], 0
    # If the utf8 sequence consist of one byte, then the range cannot be split.
    if L == 1: return [ X ], 0

    front_sequence = unicode_to_utf8(X.begin)
    back_sequence  = unicode_to_utf8(X.end - 1)
    p      = find_first_diff_byte(front_sequence, back_sequence)
    result = []
    current_begin = X.begin
    byte_sequence = copy(front_sequence)
    byte_indeces  = range(p + 1, L)
    byte_indeces.reverse()
    for q in byte_indeces:
        # There **must** be at least one overrun, even for 'q=p+1', since 'p+1' 
        # indexes the first byte after the first byte that was different. If 'p' 
        # indexed that last byte this block is never entered.
        byte_sequence[q] = max_byte_value(q)
        current_end      = utf8_to_unicode(byte_sequence) + 1
        result.append(Interval(current_begin, current_end))
        current_begin    = current_end

    if front_sequence[p] + 1 != back_sequence[p]:
        if p == L - 1: byte_sequence[p] = back_sequence[p]
        else:          byte_sequence[p] = back_sequence[p] - 1 
        current_end      = utf8_to_unicode(byte_sequence) + 1
        result.append(Interval(current_begin, current_end))
        current_begin    = current_end

    byte_sequence[p] = back_sequence[p]
    for q in range(p + 1, L):
        if back_sequence[q] == min_byte_value(q):
            byte_sequence[q] = back_sequence[q]
        else:
            if q == L - 1: byte_sequence[q] = back_sequence[q] 
            else:          byte_sequence[q] = back_sequence[q] - 1
            current_end      = utf8_to_unicode(byte_sequence) + 1
            result.append(Interval(current_begin, current_end))
            if current_begin == X.end: break
            current_begin    = current_end
            byte_sequence[q] = back_sequence[q]

    if current_begin != X.end:
        result.append(Interval(current_begin, X.end))

    return result, p

def get_trigger_sequence_for_contigous_byte_range_interval(X, L):
    front_sequence = unicode_to_utf8(X.begin)
    back_sequence  = unicode_to_utf8(X.end - 1)
    # If the interval is contigous it must produce equal length utf8 sequences

    return [ Interval(front_sequence[i], back_sequence[i] + 1) for i in range(L) ]

# For byte n > 1, the max byte range is always 0x80-0xBF (including 0xBF)
FullRange = Interval(0x80, 0xC0)
def plug_state_sequence_for_trigger_set_sequence(sm, StartStateIdx, EndStateIdx, XList, L, DIdx):
    """Create a state machine sequence for trigger set list of the same length.

       L      Length of the trigger set list.
       DIdx   Index of first byte that differs, i.e. byte[i] == byte[k] for i, k < DIdx.
       XList  The trigger set list.

                                    .          .              .
                       [A,         B,         C,         80-BF  ] 

              [Start]--(A)-->[1]--(B)-->[2]--(C)-->[3]--(80-BF)-->[End]
    """
    global FullRange
    assert L <= 6

    s_idx = StartStateIdx
    # For the common bytes it is not essential what list is considered, take list no. 0.
    for trigger_set in XList[0][:DIdx]:
        s_idx = sm.add_transition(s_idx, trigger_set)
    # Store the last state where all bytes are the same
    sDIdx = s_idx

    # Indeces of the states that run on 'full range' (frs=full range state)
    def get_sm_index(frs_db, Key):
        result = frs_db.get(Key)
        if result is None: 
            result      = state_machine.index.get()
            frs_db[Key] = result
        return result

    frs_db = {}
    for trigger_set_seq in XList:
        # How many bytes at the end trigger on 'Min->Max'
        sbw_idx  = EndStateIdx
        last_idx = EndStateIdx
        i = L - 1
        while i > DIdx and i != 0:
            if not trigger_set_seq[i].is_equal(FullRange): break
            last_idx = get_sm_index(frs_db, i-1)
            if not sm.states.has_key(last_idx): sm.states[last_idx] = State()
            sm.add_transition(last_idx, trigger_set_seq[i], sbw_idx)
            sbw_idx = last_idx
            i -= 1

        sbw_idx = last_idx
        while i > DIdx:
            # Maybe, it has already a transition on trigger_set .. (TO DO)
            last_idx = state_machine.index.get()
            sm.add_transition(last_idx, trigger_set_seq[i], sbw_idx)
            sbw_idx = last_idx
            i -= 1

        sm.add_transition(sDIdx, trigger_set_seq[i], last_idx)

    return       

def get_unicode_range():
    return NumberSet.from_range(0, 0x110000)

def get_codec_element_range():
    """Codec element's size is 1 byte."""
    return NumberSet.from_range(0, 0x100)
