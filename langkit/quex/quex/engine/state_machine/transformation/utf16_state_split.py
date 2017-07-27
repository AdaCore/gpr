# (C) 2009 Frank-Rene Schaefer
"""
ABSTRACT:

    !! UTF16 state split is similar to UTF8 state split as shown in file !!
    !! "uf8_state_split.py". Please, read the documentation there about  !!
    !! the details of the basic idea.                                    !!

    Due to the fact that utf16 conversion has only two possible byte sequence
    lengths, 2 and 4 bytes, the state split process is significantly easier
    than the utf8 state split.

    The principle idea remains: A single transition from state A to state B is
    translated (sometimes) into an intermediate state transition to reflect
    that the unicode point is represent by a value sequence.

    The special case utf16 again is easier, since, for any unicode point <=
    0xFFFF the representation value remains exactly the same, thus those
    intervals do not have to be adapted at all!
    
    Further, the identification of 'contigous' intervals where the last value
    runs repeatedly from min to max is restricted to the consideration of a
    single word. UTF16 character codes can contain at max two values (a
    'surrogate pair') coded in two 'words' (1 word = 2 bytes). The overun
    happens every 2*10 code points.  Since such intervals are pretty large and
    the probability that a range runs over multiple such ranges is low, it does
    not make sense to try to combine them. The later Hopcroft Minimization will
    not be overwhelmed by a little extra work.
"""
import os
import sys
sys.path.append(os.environ["QUEX_PATH"])

from   quex.engine.misc.utf16                    import utf16_to_unicode, unicode_to_utf16
from   quex.engine.misc.interval_handling        import Interval, NumberSet
import quex.engine.state_machine.algorithm.beautifier as beautifier

ForbiddenRange = Interval(0xD800, 0xE000)

def do(sm):
    global ForbiddenRange

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
            # -- 1st check whether a modification is necessary
            if number_set.supremum() <= 0x10000:
                sm.states[state_index].add_transition(number_set, target_state_index)
                continue

            # -- We help: General regular expressions may not bother with 
            #    the 'ForbiddenRange'. Let us be so kind and cut it here.
            number_set.subtract(ForbiddenRange)
            number_set.cut_lesser(0)
            number_set.cut_greater_or_equal(0x110000)

            # -- Add intermediate States
            #    We take the intervals with 'PromiseToTreatWellF' even though they
            #    are changed. This is because the intervals would be lost anyway
            #    after the state split, so we use the same memory and do not 
            #    cause a time consuming memory copy and constructor calls.
            interval_list = number_set.get_intervals(PromiseToTreatWellF=True)

            for interval in interval_list:
                create_intermediate_states(sm, state_index, target_state_index, interval)
    
    result = beautifier.do(sm)
    return result

def do_set(NSet):
    """Unicode values > 0xFFFF are translated into byte sequences, thus, only number
       sets below that value can be transformed into number sets. They, actually
       remain the same.
    """
    for interval in NSet.get_intervals(PromiseToTreatWellF=True):
        if interval.end > 0x10000: return None
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
    front_chunk_n = len(unicode_to_utf16(front))
    back_chunk_n  = len(unicode_to_utf16(back))
    if front_chunk_n != back_chunk_n: return None
    else:                             return front_chunk_n

def create_intermediate_states(sm, StartStateIdx, EndStateIdx, X):
    # Split the interval into a range below and above 0xFFFF. This corresponds
    # unicode values that are represented in utf16 via 2 and 4 bytes (1 and 2 words).
    interval_1word, intervals_2word = get_contigous_intervals(X)

    if interval_1word is not None:
        sm.add_transition(StartStateIdx, interval_1word, EndStateIdx)

    if intervals_2word is not None:
        for interval in intervals_2word:
            # Introduce intermediate state
            trigger_seq = get_trigger_sequence_for_interval(interval)
            s_idx = sm.add_transition(StartStateIdx, trigger_seq[0])
            sm.add_transition(s_idx, trigger_seq[1], EndStateIdx)

def get_contigous_intervals(X):
    """Split Unicode interval into intervals where all values
       have the same utf16-byte sequence length. This is fairly 
       simple in comparison with utf8-byte sequence length: There
       are only two lengths: 2 bytes and 2 x 2 bytes.

       RETURNS:  [X0, List1]  

                 X0   = the sub-interval where all values are 1 word (2 byte)
                        utf16 encoded. 
                         
                        None => No such interval
                
                List1 = list of contigous sub-intervals where coded as 2 words.

                        None => No such intervals
    """
    global ForbiddenRange
    if X.begin == -sys.maxint: X.begin = 0
    if X.end   == sys.maxint:  X.end   = 0x110000
    assert X.end != X.begin     # Empty intervals are nonsensical
    assert X.end <= 0x110000    # Interval must lie in unicode range
    assert not X.check_overlap(ForbiddenRange) # The 'forbidden range' is not to be covered.

    if   X.end   <= 0x10000: return [X, None]
    elif X.begin >= 0x10000: return [None, split_contigous_intervals_for_surrogates(X.begin, X.end)]
    else:                    return [Interval(X.begin, 0x10000), split_contigous_intervals_for_surrogates(0x10000, X.end)]

def split_contigous_intervals_for_surrogates(Begin, End):
    """Splits the interval X into sub interval so that no interval runs over a 'surrogate'
       border of the last word. For that, it is simply checked if the End falls into the
       same 'surrogate' domain of 'front' (start value of front = Begin). If it does not
       an interval [front, end_of_domain) is split up and front is set to end of domain.
       This procedure repeats until front and End lie in the same domain.
    """
    global ForbiddenRange
    assert Begin >= 0x10000
    assert End   <= 0x110000
    assert End   > Begin

    front_seq = unicode_to_utf16(Begin)
    back_seq  = unicode_to_utf16(End - 1)

    # (*) First word is the same.
    #     Then,
    #       -- it is either a one word character.
    #       -- it is a range of two word characters, but the range 
    #          extends in one contigous range in the second surrogate.
    #     In both cases, the interval is contigous.
    if front_seq[0] == back_seq[0]:
        return [Interval(Begin, End)]

    # (*) First word is NOT the same
    # Separate into three domains:
    #
    # (1) Interval from Begin until second surrogate hits border 0xE000
    # (2) Interval where the first surrogate inreases while second 
    #     surrogate iterates over [0xDC00, 0xDFFF]
    # (3) Interval from begin of last surrogate border to End
    result = []
    end    = utf16_to_unicode([front_seq[0], ForbiddenRange.end - 1]) + 1

    
    # (1) 'Begin' until second surrogate hits border 0xE000
    #      (The following **must** hold according to entry condition about 
    #       front and back sequence.)
    assert End > end
    result.append(Interval(Begin, end))

    if front_seq[0] + 1 != back_seq[0]: 
        # (2) Second surrogate iterates over [0xDC00, 0xDFFF]
        mid_end = utf16_to_unicode([back_seq[0] - 1, ForbiddenRange.end - 1]) + 1
        #     (The following **must** hold according to entry condition about 
        #      front and back sequence.)
        assert mid_end > end
        result.append(Interval(end, mid_end)) 
        end     = mid_end
         
    # (3) Last surrogate border to End
    if End > end:
        result.append(Interval(end, End)) 

    return result
    
def get_trigger_sequence_for_interval(X):
    # The interval either lies entirely >= 0x10000 or entirely < 0x10000
    assert X.begin >= 0x10000 or X.end < 0x10000

    # An interval below < 0x10000 remains the same
    if X.end < 0x10000: return [ X ]
    
    # In case that the interval >= 0x10000 it the value is split up into
    # two values.
    front_seq = unicode_to_utf16(X.begin)
    back_seq  = unicode_to_utf16(X.end - 1)

    return [ Interval(front_seq[0], back_seq[0] + 1), 
             Interval(front_seq[1], back_seq[1] + 1) ]

def get_unicode_range():
    return NumberSet.from_range(0, 0x110000)

def get_codec_element_range():
    """Codec element's size is 2 bytes."""
    return NumberSet.from_range(0, 0x10000)
