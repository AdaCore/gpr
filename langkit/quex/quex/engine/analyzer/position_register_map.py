from quex.blackboard import E_TransitionN

from collections import defaultdict
from operator    import itemgetter, attrgetter

def do(analyzer):
    """
    RETURNS: 
    
    A dictionary that maps:

            post-context-id --> position register index

    where post-context-id == E_PostContextIDs.NONE means
    'last_acceptance_position'.  The position register index starts from
    0 and ends with N, where N-1 is the number of required position
    registers. It can directly be used as index into an array of
    positions.

    -----------------------------------------------------------------------
   
    Under some circumstances it is necessary to store the acceptance
    position or the position in the input stream where a post context
    begins. For this an array of positions is necessary, e.g.

        QUEX_POSITION_LABEL     positions[4];

    where the last acceptance input position or the input position of
    post contexts may be stored. The paths of a state machine, though,
    may allow to store multiple positions in one array location, because

        (1) the path from store to restore does not intersect, or

        (2) they store their positions in exactly the same states.

    A more general and conclusive condition will be derived later. Now,
    consider the following example:
                                   . b .
                                  /     \ 
                                  \     /
             .-- a -->( 1 )-- b -->( 2 )-- c -->((3))
            /            S47                       R47 
        ( 0 )
            \            S11                       R11
             '-- b -->( 4 )-- c -->( 5 )-- d -->((6))
                          \                     /
                           '-------- e --------'

    The input position needs to be stored for post context 47 in state 1 and
    for post context 11 in state 4. Since the paths of the post contexts do
    not cross it is actually not necessary to have to separate array
    registers. One register for post context 47 and 11 is enough.  Reducing
    position registers saves space, but moreover, it may spare the
    computation time to store redundant input positions.

    .-------------------------------------------------------------------------.
    | CONDITION:                                                              |
    |                                                                         |
    | Let 'A' be a state that restores the input position from register 'x'.  |
    | If 'B' be the last state on a trace to 'A' where the position is stored |
    | in 'x'. If another state 'C' stores the input position in register 'y'  |
    | and comes **AFTER** 'B' on the trace, then 'x' and 'y' cannot be the    |
    | same.                                                                   |
    '-------------------------------------------------------------------------'
    """
    cannot_db       = get_cannot_db(analyzer)
    combinable_list = get_combinable_candidates(cannot_db)

    if True:
        result = get_mapping(combinable_list)
    else:
        # The 'Dumb' solution (for debugging)
        # Each pattern gets is own position register.
        result = dict((acceptance_id, i) for i, acceptance_id in enumerate(cannot_db.iterkeys()))

    return result

def get_cannot_db(analyzer):
    """
    Determine for each position register (identified by acceptance_id) the set of 
    position register. The condition for this is given at the entrance of this file.

    RETURNS:   
    
        map:  
              acceptance_id --> list of pattern_ids that it cannot be combined with.
    """
    # Database that maps for each state with post context id with which post context id
    # it cannot be combined.
    cannot_db = defaultdict(set)

    def cannot_db_update(db, PositionInfoList):
        """According to the CONDITION mentioned in the entry, it determined 
           what post contexts cannot be combined for the given trace list.
           Note, that FAILURE never needs a position register. After FAILURE, 
           the position is set to lexeme start plus one.
        """
        # FAILURE is excluded implicitly, since then 'transition_n_since_positioning'
        # is equal to 'LEXEME_START_PLUS_ONE' and not 'VOID'.
        entry_list = [
             x for x in PositionInfoList \
               if x.transition_n_since_positioning == E_TransitionN.VOID
        ] 

        for i, x in enumerate(entry_list):
            # Ensure, the database has at least one entry.
            if not db.has_key(x.acceptance_id): db[x.acceptance_id] = set()
            for y in entry_list[i+1:]:
                # If the positioning state differs, and we need to restore here, 
                # then the position register cannot be shared.
                if x.positioning_state_index_set == y.positioning_state_index_set: continue
                # Note, that in particular if x == y, it is left out of consideration
                db[x.acceptance_id].add(y.acceptance_id)
                db[y.acceptance_id].add(x.acceptance_id)

    for state_index, paths_info in analyzer.trace_db.iteritems():
        cannot_db_update(cannot_db, paths_info.positioning_info())

    return cannot_db

def get_combinable_candidates(cannot_db):
    """Determine sets of combinations that are allowed."""

    all_post_context_id_list = set(cannot_db.iterkeys())

    combinable_list = []
    done_set        = set()
    for acceptance_id, cannot_set in cannot_db.iteritems():
        candidate_list = list(all_post_context_id_list.difference(cannot_set))
        assert acceptance_id in candidate_list

        # Delete all candidates that cannot be be combined with the remainder
        # Consider those patterns first that have the largest set of 'cannot-s'.
        candidate_list.sort(key=lambda x: -len(cannot_db[x]))
        i    = 0
        size = len(candidate_list)
        while i < size:
            candidate  = candidate_list[i]
            cannot_set = cannot_db[candidate]
            if      ((candidate == acceptance_id) or (cannot_set.isdisjoint(candidate_list))) \
                and (candidate not in done_set):
                i += 1                # candidate can stay, go to next
            else:
                del candidate_list[i] # candidate is deleted
                size -= 1

        if len(candidate_list) != 0:
            combinable_list.append(set(candidate_list))
            done_set.update(candidate_list)

    return combinable_list

def get_mapping(combinable_list):
    """Determine the mapping from acceptance_id to the register id that can be used
       to index into an array.
    """
    result      = {}
    array_index = 0
    while len(combinable_list) != 0:
        # Allways, try to combine the largest combinable set first.
        k           = max(enumerate(combinable_list), key=lambda x: len(x[1]))[0]
        combination = combinable_list.pop(k)

        for acceptance_id in (x for x in combination if not result.has_key(x)):
            result[acceptance_id] = array_index

        # Since: -- The combinations only contain post_context_id's that have not been
        #           mentioned before, and
        #        -- all empty combinations are deleted from the combinable_list,
        # Thus:  It is safe to assume that new entries were made for the current
        #        array index. Thus, a new array index is required for the next turn.
        array_index += 1

        # Delete combinations that have become empty
        size = len(combinable_list)
        p    = 0
        while p < size:
            if len(combinable_list[p]) == 0: del combinable_list[p]; size -= 1
            else:                            p += 1

    return result

def print_this(TheAnalyzer):
    for state_index, paths_info in sorted(TheAnalyzer.trace_db.iteritems(),key=itemgetter(0)):
        position_info = paths_info.positioning_info()
        print "State %i:" % state_index
        txt = ""
        for x in sorted(position_info, key=attrgetter("acceptance_id")): 
            if x.transition_n_since_positioning == E_TransitionN.VOID:
                txt += "    (*) "
            else: 
                txt += "        "
            txt += "[%7s]: %s/%s\n" % (x.acceptance_id, x.pre_context_id, x.positioning_state_index_set)
        print txt

