from   quex.engine.operations.operation_list import OpList

from   quex.blackboard import E_StateIndices, \
                              E_TriggerIDs

from   collections     import namedtuple

class TransitionID(namedtuple("TransitionID_tuple", ("target_state_index", "source_state_index", "trigger_id"))):
    """Objects of this type identify a transition. 
    
                   .----- trigger_id ---->-[ TransitionAction ]----.
                   |                                               |
        .--------------------.                          .--------------------.
        | source_state_index |                          | target_state_index |
        '--------------------'                          '--------------------'
    
       NOTE: There might be multiple transitions from source to target. Each transition
             has another trigger_id. The relation between a TransitionID and a 
             TransitionAction is 1:1.

    """
    def __new__(self, StateIndex, FromStateIndex, TriggerId):
        assert isinstance(StateIndex, (int, long))     or StateIndex     in E_StateIndices
        assert isinstance(FromStateIndex, (int, long)) or FromStateIndex in E_StateIndices
        assert isinstance(TriggerId, (int, long))      or TriggerId      in E_TriggerIDs
        return super(TransitionID, self).__new__(self, StateIndex, FromStateIndex, TriggerId)

    def is_from_reload(self):
        return   self.source_state_index == E_StateIndices.RELOAD_FORWARD \
              or self.source_state_index == E_StateIndices.RELOAD_BACKWARD

    def __repr__(self):
        source_state_str = "%s" % self.source_state_index

        if self.trigger_id == 0:
            return "TransitionID(to=%s, from=%s)" % (self.target_state_index, source_state_str)
        else:
            return "TransitionID(to=%s, from=%s, trid=%s)" % (self.target_state_index, source_state_str, self.trigger_id)

class TransitionAction(object):
    """Object containing information about commands to be executed upon
       transition into a state.

       .command_list  --> list of commands to be executed upon the transition.
       .door_id       --> An 'id' which is supposed to be unique for a command list. 
                          It is (re-)assigned during the process of 
                          'EntryActionDB.categorize()'.
    """
    __slots__ = ("door_id", "_command_list")
    def __init__(self, InitOpList=None):
        # NOTE: 'DoorId' is not accepted as argument. Is needs to be assigned
        #       by '.categorize()' in the action_db. Then, transition actions
        #       with the same OpList-s share the same DoorID.
        assert InitOpList is None or isinstance(InitOpList, OpList), "%s: %s" % (InitOpList.__class__, InitOpList)
        self.door_id = None 
        if InitOpList is None: self._command_list = OpList() 
        else:                       self._command_list = InitOpList
 
    @property
    def command_list(self): 
        return self._command_list

    @command_list.setter
    def command_list(self, CL): 
        assert isinstance(CL, OpList) or CL is None
        self._command_list = CL
        
    def clone(self):
        result = TransitionAction(self._command_list.clone())
        result.door_id = self.door_id  # DoorID-s are immutable
        return result

    # Make TransitionAction usable for dictionary and set
    def __hash__(self):      
        return hash(self._command_list)

    def __eq__(self, Other):
        return self._command_list == Other._command_list

    def __repr__(self):
        return "(%s: [%s])" % (self.door_id, self._command_list)

