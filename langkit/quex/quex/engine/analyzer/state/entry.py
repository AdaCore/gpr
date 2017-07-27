from   quex.engine.operations.operation_list                  import Op, OpList
from   quex.engine.analyzer.state.entry_action    import TransitionID, TransitionAction
from   quex.engine.analyzer.door_id_address_label import dial_db, \
                                                         DoorID
from   quex.engine.misc.tools                     import typed, \
                                                         TypedDict
from   quex.blackboard                            import E_Op, \
                                                         E_StateIndices, \
                                                         E_DoorIdIndex

class Entry(object):
    """________________________________________________________________________

    An Entry object stores commands to be executed at entry into a state
    depending on a particular source state; and may be also depending on
    the particular trigger.
    
    BASICS _________________________________________________________________

    To keep track of entry actions, OpList-s need to 
    be associated with a TransitionID-s, i.e. pairs of (state_index,
    from_state_index). This happens in the member '.__db', i.e.
    
       .action_db:    TransitionID --> TransitionAction

    where a TransitionID consists of: .from_state_index
                                      .state_index
                                      .trigger_id 
 
    and a TransitionAction consists of: .door_id
                                        .command_list
                                        
    where '.door_id'  identifies a specific door  of the  entry into  the state.
    It is distinctly associated with a list of commands '.command_list'. The 
    commands of '.command_list' are executed if the state is entered by
    a transition given with the key's TransitionID. A call to 

                            action_db.categorize()

    ensures that
                                  1     1
                        DoorID  <---------> OpList
   
    In words: 
    
       -- each TransitionAction *has a* valid door_id. That is, every list of
          commands is identified with a door_id.

       -- The door_id *distinctly* determines the command list (in the entry). 
          That is, door_id-s of TransitionAction-s differ if and only if their
          command lists are different.
                  
    Later on in the code generation, a 'door tree' is generated to produce
    optimized code which profits from common commands in command lists. But,
    for now, it is important to remember:

              .---------------------------------------------------.
              |  A DoorID distinctly identifies a OpList to  |
              |       be executed the at entry of a state.        |
              '---------------------------------------------------'
    """

    __slots__ = ("__db", "__largest_used_door_sub_index", "__trigger_id_db")

    def __init__(self):
        self.__db                          = TypedDict(TransitionID, TransitionAction)
        self.__largest_used_door_sub_index = 0    # '0' is used for 'Door 0', i.e. reload entry
        self.__trigger_id_db               = {}   # (ToState, FromState) --> max. used trigger_id

    def get(self, TheTransitionID):
        return self.__db.get(TheTransitionID)

    def get_action(self, StateIndex, FromStateIndex, TriggerId=0):
        return self.__db.get(TransitionID(StateIndex, FromStateIndex, TriggerId))

    def get_command_list(self, StateIndex, FromStateIndex, TriggerId=0):
        action = self.__db.get(TransitionID(StateIndex, FromStateIndex, TriggerId))
        if action is None: return None
        else:              return action.command_list

    def get_command_list_by_door_id(self, DoorId):
        for action in self.__db.itervalues():
            if action.door_id == DoorId:
                return action.command_list
        return None

    def get_door_id(self, StateIndex, FromStateIndex, TriggerId=0):
        """RETURN: DoorID of the door which implements the transition 
                          (FromStateIndex, StateIndex).
                   None,  if the transition is not implemented in this
                          state.
        """
        action = self.__db.get(TransitionID(StateIndex, FromStateIndex, TriggerId))
        if action is None: return None
        else:              return action.door_id

    def get_transition_id_list(self, DoorId):
        return [ 
           transition_id for transition_id, action in self.__db.iteritems()
                         if action.door_id == DoorId 
        ]

    def get_door_id_by_command_list(self, TheOpList):
        """Finds the DoorID of the door that implements TheOpList.
           RETURNS: None if no such door exists that implements TheOpList.
        """
        for action in self.__db.itervalues():
            if action.command_list == TheOpList:
                return action.door_id
        return None

    def absorb(self, Other):
        """Absorbs all, but the 'reload transitions'.
        """
        for tid, action in Other.__db.iteritems():
            self.enter(tid.target_state_index, tid.source_state_index, action)

        if self.__largest_used_door_sub_index < Other.__largest_used_door_sub_index:
            self.__largest_used_door_sub_index = Other.__largest_used_door_sub_index

    @typed(Cl=OpList)
    def enter_OpList(self, ToStateIndex, FromStateIndex, Cl):
        return self.enter(ToStateIndex, FromStateIndex, TransitionAction(Cl))

    @typed(TheAction=TransitionAction)
    def enter(self, ToStateIndex, FromStateIndex, TheAction):
        assert isinstance(TheAction, TransitionAction)
        #!! It is ABSOLUTELY essential, that the OpList-s related to actions are
        #!! independent! Each transition must have its OWN OpList!
        for transition_id, action in self.__db.iteritems():
            assert id(TheAction.command_list) != id(action.command_list) 

        transition_id = TransitionID(ToStateIndex, FromStateIndex, 
                                     TriggerId=self.__get_trigger_id(ToStateIndex, FromStateIndex))
        self.__db[transition_id] = TheAction
        return transition_id

    @typed(ta=TransitionAction)
    def enter_state_machine_entry(self, SM_id, ToStateIndex, ta):
        ta.door_id = DoorID.state_machine_entry(SM_id)
        return self.enter(ToStateIndex, E_StateIndices.BEFORE_ENTRY, ta)

    def enter_before(self, ToStateIndex, FromStateIndex, TheOpList):
        transition_id = TransitionID(ToStateIndex, FromStateIndex, TriggerId=0)
        ta = self.__db.get(transition_id)
        # A transition_action cannot be changed, once it has a DoorID assigned to it.
        assert ta.door_id is None
        ta.command_list = TheOpList.concatinate(ta.command_list)

    def __get_trigger_id(self, ToStateIndex, FromStateIndex):
        ft = (ToStateIndex, FromStateIndex)
        tmp = self.__trigger_id_db.get(ft)
        # "FromStateIndex == E_StateIndices.BEFORE_ENTRY" indicates the entry into the 
        # state machine. There cannot be more than one entry into the state 
        # machine. Thus, it cannot appear twice.
        assert FromStateIndex != E_StateIndices.BEFORE_ENTRY or tmp is None
        if tmp is None: self.__trigger_id_db[ft]  = 0; tmp = 0;
        else:           self.__trigger_id_db[ft] += 1
        return tmp

    def remove_transition_from_states(self, StateIndexSet):
        assert isinstance(StateIndexSet, set)
        for transition_id in self.__db.keys():
            if transition_id.source_state_index in StateIndexSet:
                del self.__db[transition_id]

    def size(self):
        return len(self.__db)

    def add_Accepter_on_all(self, PreContextID, AcceptanceID):
        """Add an acceptance at the top of each accepter at every door. If there
           is no accepter in a door it is created.
        """
        for ta in self.__db.itervalues():
            # Catch the accepter, if there is already one, if not create one.
            ta.command_list.access_accepter().add(PreContextID, AcceptanceID)

    def add_StoreInputPosition(self, StateIndex, FromStateIndex, PreContextID, PositionRegister, Offset):
        """Add 'store input position' to specific door. See 'SeStoreInputPosition'
           comment for the reason why we do not store pre-context-id.
        """
        command_list = self.__db[TransitionID(StateIndex, FromStateIndex, 0)].command_list
        cmd          = Op.StoreInputPosition(PreContextID, PositionRegister, Offset)
        # Make sure it is the first!
        command_list.insert(0, cmd)
        # Never store twice in the same position register! 
        # => Make sure, that there is no second of the same kind!
        i = len(command_list) - 1
        while i >= 1: # leave 'i=0' which has just been inserted!
            if command_list[i] == cmd:
                del command_list[i]
            i -= 1

    def has_transition(self, ToStateIndex, FromStateIndex):
        for key in self.__db.iterkeys():
            if key.target_state_index == ToStateIndex and key.source_state_index == FromStateIndex:
                return True
        return False

    def has_command(self, OpId):
        assert OpId in E_Op
        for action in self.__db.itervalues():
            if action.command_list.has_command_id(OpId): 
                return True
        return False

    def has_transitions_to_door_id(self, DoorId):
        for action in self.__db.itervalues():
            if action.door_id == DoorId:
                return True
        return False

    def door_id_set(self):
        """RETURNS: The door ids of this entry.

        In the frame of a CommandTree, this set is the set of 'leaf door ids'.
        """
        return set(action.door_id 
                   for action in self.__db.itervalues()
                   if action.door_id is not None)

    def get_state_machine_entry_door_id(self):
        """RETURNS: DoorID, if the entry contains THE entry into the analyzer.
                    None, if not.
        """
        for transition_id, action in self.__db.iteritems():
            if transition_id.source_state_index != E_StateIndices.BEFORE_ENTRY: 
                continue
            assert action.door_id.door_index == E_DoorIdIndex.STATE_MACHINE_ENTRY
            return action.door_id
        return None

    def delete(self, StateIndex, FromStateIndex, TriggerId=0):
        del self.__db[TransitionID(StateIndex, FromStateIndex, 0)]

    def replace_position_registers(self, PositionRegisterMap):
        """Originally, each pattern gets its own position register if it is
        required to store/restore the input position. The 'PositionRegisterMap'
        is a result of an analysis which tells whether some registers may
        actually be shared. This function does the replacement of positioning
        registers based on what is given in 'PositionRegisterMap'.
        """
        for action in self.__db.itervalues():
            action.command_list.replace_position_registers(PositionRegisterMap)

        return

    def delete_superfluous_commands(self):
        for action in self.__db.itervalues():
            action.command_list.delete_superfluous_commands()
        return

    def itervalues(self):
        for ta in self.__db.itervalues():
            assert ta.door_id is not None, ".categorize() needs to be called before this!"
            yield ta

    def iteritems(self):
        return self.__db.iteritems()

    def __setitem__(self, Key, Value):
        #!! It is ABSOLUTELY essential, that the OpList-s related to actions are
        #!! independent! Each transition must have its OWN OpList!
        for action in self.__db.itervalues():
            assert id(Value.command_list) != id(action.command_list)

        self.__db[Key] = Value
        return Value

    def categorize(self, StateIndex):
        """
        This function considers TransitionActions where '.door_id is None' and
        assigns them a DoorID.  A DoorID identifies (globally) the OpList
        and the state which they enter. In other words, if two transition actions
        of a state have the same command lists, they have the same DoorID. 

        RETURNS: List of newly assigned pairs of (TransitionID, DoorID)s.
        """
        #!! It is ABSOLUTELY essential, that the OpList-s related to actions are
        #!! independent! Each transition must have its OWN OpList!
        cmd_list_ids = [ id(action.command_list) for action in self.__db.itervalues() ]
        assert len(cmd_list_ids) == len(set(cmd_list_ids)) # size(list) == size(unique set)

        work_list = [
            (transition_id, action) for transition_id, action in self.__db.iteritems()
                                    if action.door_id is None
        ]

        if len(work_list) == 0:
            return

        command_list_db = dict(
            (action.command_list, action.door_id) for action in self.__db.itervalues()
                                                  if action.door_id is not None
        )

        def _get_door_id(CL):
            # If there was an action with the same command list, then assign
            # the same door id. Leave the action intact! May be, it is modified
            # later and will differ from the currently same action.
            new_door_id = command_list_db.get(CL)

            if new_door_id is not None: return new_door_id

            return dial_db.new_door_id(StateIndex)

        def sort_key(X):
            return (X[0].target_state_index, X[0].source_state_index, X[0].trigger_id)

        for transition_id, action in sorted(work_list, key=sort_key): # NOT: 'iteritems()'
            action.door_id                       = _get_door_id(action.command_list)
            command_list_db[action.command_list] = action.door_id

        assert self.check_consistency()

    @property 
    def largest_used_door_sub_index(self):
        return self.__largest_used_door_sub_index

    def check_consistency(self):
        """Any two entries with the same DoorID must have the same command list
           associated with it.
        """
        check_db = {}
        for action in self.__db.itervalues():
            if action.door_id is None: 
                continue
            cmp_command_list = check_db.get(action.door_id)
            if cmp_command_list is None: 
                check_db[action.door_id] = action.command_list
            elif cmp_command_list != action.command_list:
                return False

        # Some commands shall never occur more than once in a command list:
        # --> Op.unique_set
        for transition_action in self.__db.itervalues():
            unique_found_set = set()
            for cmd in transition_action.command_list:
                if   cmd.id not in Op.unique_set: continue
                elif cmd.id in unique_found_set:       return False
                unique_found_set.add(cmd.id)
        return True

    def get_string(self):
        txt = []
        for tid, ta in self.__db.iteritems():
            txt.append("%s:%s: {\n" % (tid, ta.door_id))
            for command in ta.command_list:
                txt.append("    " + repr(command))
            txt.append("}\n")
        return "".join(txt)

    @property
    def action_db(self):
        return self.__db

    def __hash__(self):
        xor_sum = 0
        for door in self.__db.itervalues():
            xor_sum ^= hash(door.command_list)
        return xor_sum

    def __eq__(self, Other):
        assert False, "not used"

    def is_equal(self, Other):
        assert False, "not used"

    def __repr__(self):
        def get_accepters(AccepterList):
            if len(AccepterList) == 0: return []
            assert len(AccepterList) == 1
            return [ str(AccepterList[0]) ]

        def get_storers(StorerList):
            txt = [ 
                str(cmd)
                for cmd in sorted(StorerList, key=lambda cmd: (cmd.content.pre_context_id, cmd.content.position_register))
            ]
            return txt

        def get_pre_context_oks(PCOKList):
            txt = [
                str(cmd)
                for cmd in sorted(PCOKList, key=lambda cmd: cmd.content.pre_context_id)
            ]
            return txt

        def get_set_template_state_keys(TemplateStateKeySetList):
            txt = [
                str(cmd)
                for cmd in sorted(TemplateStateKeySetList, key=lambda cmd: cmd.content.state_key)
            ]
            return txt

        def get_set_path_iterator_keys(PathIteratorSetKeyList):
            def sort_key(Op):
                return (Op.content.path_walker_id, Op.content.path_id, Op.content.offset)
            txt = [
                str(cmd)
                for cmd in sorted(PathIteratorSetKeyList, key=sort_key)
            ]
            return txt

        result = []
        for transition_id, door in sorted(self.__db.iteritems(),key=lambda x: x[0].source_state_index):
            accept_command_list = []
            store_command_list  = []
            pcok_command_list   = []
            ssk_command_list    = []
            spi_command_list    = []
            for cmd in door.command_list:
                if   cmd.id == E_Op.Accepter:            accept_command_list.append(cmd)
                elif cmd.id == E_Op.PreContextOK:        pcok_command_list.append(cmd)
                elif cmd.id == E_Op.TemplateStateKeySet: ssk_command_list.append(cmd)
                elif cmd.id == E_Op.PathIteratorSet:     spi_command_list.append(cmd)
                elif cmd.id == E_Op.StoreInputPosition:  store_command_list.append(cmd)

            result.append("    .from %s:" % repr(transition_id.source_state_index).replace("L", ""))
            a_txt  = get_accepters(accept_command_list)
            s_txt  = get_storers(store_command_list)
            p_txt  = get_pre_context_oks(pcok_command_list)
            sk_txt = get_set_template_state_keys(ssk_command_list)
            pi_txt = get_set_path_iterator_keys(spi_command_list)
            content = "".join("%s\n" % x for x in a_txt + s_txt + p_txt + sk_txt + pi_txt)
            content = content.strip()
            if content.count("\n") == 0:
                # Append to same line
                content = " " + content
            else:
                # Indent properly
                content = "\n        " + content.replace("\n", "\n        ") 
            result.append("%s\n" % content)

        if len(result) == 0: return ""
        #return "-X--\n%s\n-X--" % "".join(result)
        return "".join(result)

