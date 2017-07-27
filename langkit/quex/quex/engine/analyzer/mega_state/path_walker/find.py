from   quex.engine.analyzer.mega_state.path_walker.path  import CharacterPath
from   quex.engine.misc.tools                                 import flatten_list_of_lists
from   quex.engine.misc.tree_walker                      import TreeWalker
from   quex.blackboard                                   import E_Compression, E_StateIndices

from   operator    import xor

def do(TheAnalyzer, CompressionType, AvailableStateIndexSet):
    """Starting point of the search for single character traces in the 
    state machine (TheAnalyzer). For each state in the state machine
    try to find branches of paths. 
    
    States which are closer to the init state are searched first. This 
    way quickly a set can be build of longest paths, which make searches
    from follower states unnecessary.
    """
    # depth_db: state_index ---> distance from init state.
    # We first search for the longest paths, so that searches for sub paths
    # become unnecessary. This way computation time is reduced.
    depth_db = TheAnalyzer.get_depth_db()

    iterable_state_indices = (                                                \
        i for i in TheAnalyzer.state_db.iterkeys()                            \
        if i in AvailableStateIndexSet and i != TheAnalyzer.init_state_index \
    )

    path_list = flatten_list_of_lists(
        CharacterPathList_find(TheAnalyzer, state_index, CompressionType, AvailableStateIndexSet)
        for state_index in sorted(iterable_state_indices, key=lambda i: depth_db[i])
    )

    return path_list

def DropOutConsideration_relate(PathWalkersDropOutDoorId, Target):
    if not Target.drop_out_f(): return None
    else:                       return PathWalkersDropOutDoorId

def DropOutConsideration_cmp(DoorId_A, DoorId_B):
    """For transition maps on path DropOut == DropOut. At the end of the
    transition map the drop-outs are distinguished by the state key.
    """
    if xor((DoorId_A == E_StateIndices.VOID), (DoorId_B == E_StateIndices.VOID)):
        return False # See 'match_with_wildcard' for further consideration.
    elif   DoorId_A.state_index == E_StateIndices.RELOAD_FORWARD \
       and DoorId_B.state_index == E_StateIndices.RELOAD_FORWARD:
        return True
    elif   DoorId_A.state_index == E_StateIndices.RELOAD_BACKWARD \
       and DoorId_B.state_index == E_StateIndices.RELOAD_BACKWARD:
        return True
    elif   DoorId_A.drop_out_f() and DoorId_B.drop_out_f():
        return True
    else:
        return DoorId_A == DoorId_B

def CharacterPathList_find(analyzer, StateIndex, CompressionType, AvailableStateIndexSet):
    """Searches for the BEGINNING of a path, i.e. a single character transition
    to a subsequent state. If such a transition is found, a search for a path
    is initiated using the 'PathFinder'.

    This function itself it not recursive. PathFinder does a Depth-First Search.
    """
    result_list = []

    State          = analyzer.state_db[StateIndex]
    target_map     = State.map_target_index_to_character_set
    transition_map = State.transition_map 

    for target_idx, trigger_set in target_map.iteritems():
        if   target_idx not in AvailableStateIndexSet: continue # State is not an option.
        elif target_idx == StateIndex:                 continue # Recursion! Do not go further!

        # Only single character transitions can be element of a path.
        transition_char = trigger_set.get_the_only_element()
        if transition_char is None:                    continue # Not a single char transition.

        path_finder  = PathFinder(analyzer, CompressionType, AvailableStateIndexSet)
        target_state = analyzer.state_db[target_idx]
        path         = CharacterPath(State, transition_map, transition_char)
        path_finder.do((path, target_state))

        result_list.extend(path_finder.result)

    return result_list

class PathFinder(TreeWalker):
    """
    This function is its nature recursive. To avoid problems with stack size,
    it relies on 'TreeWalker'.

    Recursive search for single character transition paths inside the given
    state machine.  Assume, that a first single character transition has been
    found.  As a result, a CharacterPath object must have been created which
    contains a 'wild card', i.e.  a character that is left to be chosen freely,
    because it is covered by the first transition character.

    RECURSION STEP: 
    
            -- Add current state add the end of the given path.
            -- If required: Plug the wildcard.

    BRANCH: Branch to all follow-up states where:

            -- There is a single character transition to them.
            -- The state itself is not part of the path yet.
               Loops cannot be modelled by a PathWalkerState.
            -- The transition map fits the transition map of the 
               given path.

    TERMINAL: There are no further single character transitions which
              meet the aforementioned criteria.
    """
    def __init__(self, TheAnalyzer, CompressionType, AvailableStateIndexSet):
        self.__depth       = 0
        self.analyzer      = TheAnalyzer
        self.available_set = AvailableStateIndexSet
        self.uniform_f     = (CompressionType == E_Compression.PATH_UNIFORM)
        self.result        = []
        TreeWalker.__init__(self)

    def on_enter(self, Args):
        path           = Args[0]  # 'CharacterPath'
        State          = Args[1]  # 'AnalyzerState'
        transition_map = State.transition_map 

        # We are searching on follow states of this 'State'. 
        # => 'State' will becomes an element of the path.
        #    (Its target state still will remain an external 'Terminal')
        # => Entry and DropOut of 'State' are implemented as part of the 
        #    path walker.
        
        # If uniformity is required, then this is the place to check for it.
        if self.uniform_f and not path.uniformity_with_predecessor(State):
            # TERMINATION _________________________________________________
            self.result_add(path, State.index)
            return None

        # BRANCH __________________________________________________________
        sub_list = []
        for target_index, trigger_set in State.map_target_index_to_character_set.iteritems():
            if target_index not in self.available_set: continue

            # Only single character transitions can be element of a path.
            transition_char = trigger_set.get_the_only_element()
            if transition_char is None: continue

            # A PathWalkerState cannot implement a loop.
            if path.contains_state(target_index): continue # Loop--don't go!

            target_state = self.analyzer.state_db[target_index]

            # TransitionMap matching? 
            plug = path.transition_map.match_with_wildcard(transition_map, transition_char, EqualCmp=DropOutConsideration_cmp)
            if   plug is None:
                continue # No match possible 
            elif plug > 0  and not path.has_wildcard(): 
                continue # Wilcard required for match, but there is no wildcard open.
            new_path = path.extended_clone(State, transition_char, plug) 

            # RECURSION STEP ______________________________________________
            # Clone the current state and append the new terminal.
            sub_list.append((new_path, target_state))

        # TERMINATION _____________________________________________________
        if len(sub_list) == 0:
            self.result_add(path, State.index)
            return None

        return sub_list

    def on_finished(self, Args):
        self.__depth -= 1

    def result_add(self, path, TerminalStateIndex):
        if len(path.step_list) <= 1: return
        path.finalize(TerminalStateIndex)
        self.result.append(path)


