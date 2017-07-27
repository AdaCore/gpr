import quex.engine.state_machine.index  as     sm_index
from   quex.engine.misc.tools                import print_callstack, TypedDict, TypedSet
from   quex.blackboard                  import E_IncidenceIDs, E_StateIndices, E_DoorIdIndex

from   collections import namedtuple
#______________________________________________________________________________
# Label:
#
# Identifier in the generated source code which can be 'gotoed'. A label is
# distinctly linked with a DoorID and an Address, i.e.
#______________________________________________________________________________
#
# Address:
#
# Numeric representation of a label. Using an address a variable may contain
# the information of what label to go, and the goto is then executed by a
# code fragment as
#
#      switch( address_variable ) {
#         ...
#         case 0x4711:  goto _4711;
#         ...
#      }
#______________________________________________________________________________
#
# TransitionID:
#
# Identifies a transition from one source to target state.  There may be
# multiple transitions for the same source-target pair. Each one identified by
# an additional 'trigger_id'.  TransitionIDs are connected with OpList-s
# at entry into a state; But 
#
#                               n          1
#               TransitionID  <--------------> OpList
#
# That is, there may be multiple TransitionID-s with the same OpList.
# TransitionID-s are useful during the construction of entries.
#______________________________________________________________________________


#______________________________________________________________________________
#
# DoorID:
#
# Marks an entrance into a 'Processor', an AnalyzerState for example.  A
# Processor can have multiple entries, each entry has a different DoorID. A
# DoorID identifies distinctly a OpList to be executed upon entry.
# No two OpList-s
# are the same except that their DoorID is the same.
#            
#______________________________________________________________________________
class DoorID(namedtuple("DoorID_tuple", ("state_index", "door_index"))):
    def __new__(self, StateIndex, DoorIndex, PlainF=False):
        assert isinstance(StateIndex, (int, long)) or StateIndex in E_StateIndices or StateIndex == E_IncidenceIDs.MATCH_FAILURE
        # 'DoorIndex is None' --> right after the entry commands (targetted after reload).
        assert isinstance(DoorIndex, (int, long))  or DoorIndex is None or DoorIndex in E_DoorIdIndex, "%s" % DoorIndex

        if PlainF:
            return super(DoorID, self).__new__(self, StateIndex, DoorIndex)

        # If the DoorID object already exists, than do not generate a second one.
        result = dial_db.access_door_id(StateIndex, DoorIndex)
        if result is not None:
            return result

        # Any created DoorID must be properly registered.
        result = super(DoorID, self).__new__(self, StateIndex, DoorIndex)
        dial_db.register_door_id(result)

        return result

    @staticmethod
    def drop_out(StateIndex):              return DoorID(E_StateIndices.DROP_OUT, StateIndex)
    @staticmethod                        
    def transition_block(StateIndex):      return DoorID(StateIndex, E_DoorIdIndex.TRANSITION_BLOCK)
    @staticmethod                        
    def incidence(IncidenceId):            return DoorID(dial_db.map_incidence_id_to_state_index(IncidenceId), E_DoorIdIndex.ACCEPTANCE)
    @staticmethod                        
    def bipd_return(IncidenceId):     return DoorID(dial_db.map_incidence_id_to_state_index(IncidenceId), E_DoorIdIndex.BIPD_RETURN)
    @staticmethod                        
    def state_machine_entry(SM_Id):        return DoorID(SM_Id,      E_DoorIdIndex.STATE_MACHINE_ENTRY)
    @staticmethod                        
    def global_state_router():             return DoorID(0L,         E_DoorIdIndex.GLOBAL_STATE_ROUTER)
    @staticmethod                         
    def global_end_of_pre_context_check(): return DoorID(0L,         E_DoorIdIndex.GLOBAL_END_OF_PRE_CONTEXT_CHECK)
    @staticmethod
    def global_reentry():                  return DoorID(0L,         E_DoorIdIndex.GLOBAL_REENTRY)
    @staticmethod
    def return_with_on_after_match():      return DoorID(0L,         E_DoorIdIndex.RETURN_WITH_ON_AFTER_MATCH)
    @staticmethod
    def continue_with_on_after_match():    return DoorID(0L,         E_DoorIdIndex.CONTINUE_WITH_ON_AFTER_MATCH)
    @staticmethod
    def continue_without_on_after_match(): return DoorID(0L,         E_DoorIdIndex.CONTINUE_WITHOUT_ON_AFTER_MATCH)

    def drop_out_f(self):                  return self.state_index == E_StateIndices.DROP_OUT
    def last_acceptance_f(self):           return     self.door_index  == E_DoorIdIndex.ACCEPTANCE \
                                                  and self.state_index == E_IncidenceIDs.VOID

    def __repr__(self):
        return "DoorID(s=%s, d=%s)" % (self.state_index, self.door_index)

#______________________________________________________________________________
# DialDB: DoorID, Address, Label - Database
#
# For a given Label, a given Address, or a given DoorID, there the remaining
# two a distinctly defined. That is,
#
#                           1     n         
#               StateIndex <-------> DoorID 
#                                   1 /   \ 1
#                                    /     \
#                                   /       \
#                                1 /         \ 1
#                           Address <-------> Label
#                                    1     1
# 
# The DialDB maintains the injective relationships and finds equivalents
# for one given element. All addresses/labels/door-ids relate to a state
# with a given StateIndex. 
#______________________________________________________________________________
AddressLabelPair = namedtuple("AddressLabelPair_tuple", ("address", "label"))

class DialDB(object):
    __slots__ = ( "__d2la", "__door_id_db", "__gotoed_address_set", "__routed_address_set", "__address_i", "__incidence_id_i", "__map_incidence_id_to_state_index" )
    def __init__(self):
        self.clear()

    def __debug_address_generation(self, DoorId, Address, *SuspectAdrList):
        """Prints the callstack if an address of SuspectAdrList is generated.
        """
        if Address not in SuspectAdrList:
            return
        print "#DoorID %s <-> Address %s" % (DoorId, Address)
        print_callstack()

    def __debug_address_usage(self, Address, *SuspectAdrList):
        """Prints the callstack if an address of SuspectAdrList is generated.
        """
        if Address not in SuspectAdrList:
            return
        print "#Used %s" % Address
        print_callstack()

    def clear(self):
        # Database: [DoorID] [Address] [Label] 
        # 
        # The database is represented by a dictionary that maps:
        #
        #               DoorID --> tuple(Address, Label)
        #
        self.__d2la = TypedDict(DoorID, AddressLabelPair)

        # Track all generated DoorID objects with 2d-dictionary that maps:
        #
        #          StateIndex --> ( DoorSubIndex --> DoorID )
        #
        # Where the DoorID has the 'state_index' and 'door_index' equal to
        # 'StateIndex' and 'DoorSubIndex'.
        #
        self.__door_id_db = {} # TypedDict(long, dict)
       
        # Track addresses which are subject to 'goto' and those which need to
        # be routed.
        self.__gotoed_address_set = TypedSet(long)
        self.__routed_address_set = TypedSet(long)

        # Address counter to generate unique addresses
        self.__address_i = long(-1)

        # Unique incidence id inside a mode
        self.__incidence_id_i = long(-1)

        # Mapping from incidence_id to terminal state index
        self.__map_incidence_id_to_state_index = {}

    def routed_address_set(self):
        return self.__routed_address_set

    def gotoed_address_set(self):
        return self.__gotoed_address_set

    def label_is_gotoed(self, Label):
        address = self.get_address_by_label(Label)
        return address in self.__gotoed_address_set

    def __new_entry(self, StateIndex=None, DoorSubIndex=None):
        """Create a new entry in the database. First, a DoorID is generated.
        Then a new set of address and label is linked to it. The link between
        DoorID and AddressLabelPair is stored in '.__d2la'. A list of existing
        DoorID-s is maintained in '.__door_id_db'.
        """
        def specify(StateIndex, DoorSubIndex):
            if StateIndex is None:   state_index = sm_index.get() # generate a new StateIndex
            else:                    state_index = StateIndex
            if DoorSubIndex is None: door_sub_index = self.max_door_sub_index(state_index) + 1
            else:                    door_sub_index = DoorSubIndex
            return state_index, door_sub_index

        state_index, door_sub_index = specify(StateIndex, DoorSubIndex)
        door_id                     = DoorID(state_index, door_sub_index, PlainF=True)
        address_label_pair          = self.register_door_id(door_id)

        return door_id, address_label_pair

    def max_door_sub_index(self, StateIndex):
        """RETURN: The greatest door sub index for a given StateIndex. 
                   '-1' if not index has been used yet.
        """
        result = - 1
        sub_db = self.__door_id_db.get(StateIndex)
        if sub_db is None: return result

        for dsi in (x for x in sub_db.iterkeys() if isinstance(x, (int, long))):
            if dsi > result: result = dsi
        return result

    def register_door_id(self, DoorId):
        self.__address_i += 1
        address_label_pair   = AddressLabelPair(self.__address_i, "_%i" % self.__address_i)

        if False: # True/False activates debug messages
            self.__debug_address_generation(DoorId, self.__address_i, 18)

        self.__d2la[DoorId] = address_label_pair

        sub_db = self.__door_id_db.get(DoorId.state_index)
        if sub_db is None:
            self.__door_id_db[DoorId.state_index] = { DoorId.door_index: DoorId }
        else:
            assert DoorId.door_index not in sub_db # Otherwise, it would not be new
            sub_db[DoorId.door_index] = DoorId

        return address_label_pair

    def access_door_id(self, StateIndex, DoorSubIndex):
        """Try to get a DoorID from the set of existing DoorID-s. If a DoorID
        with 'StateIndex' and 'DoorSubIndex' does not exist yet, then create it.
        """
        sub_db = self.__door_id_db.get(StateIndex)
        if sub_db is None:
            return self.new_door_id(StateIndex, DoorSubIndex)

        door_id = sub_db.get(DoorSubIndex)
        if door_id is None:
            return self.new_door_id(StateIndex, DoorSubIndex)
        else:
            return door_id

    def new_incidence_id(self):
        self.__incidence_id_i += 1
        return self.__incidence_id_i

    def new_door_id(self, StateIndex=None, DoorSubIndex=None):
        door_id, alp = self.__new_entry(StateIndex, DoorSubIndex)
        return door_id

    def new_address(self, StateIndex=None):
        door_id, alp = self.__new_entry(StateIndex)
        return alp.address

    def new_label(self, StateIndex=None):
        door_id, alp = self.__new_entry(StateIndex)
        return alp.label

    def get_label_by_door_id(self, DoorId, GotoedF=False):
        assert DoorId in self.__d2la, "%s" % str(DoorId)
        address, label = self.__d2la[DoorId]
        if GotoedF:
            self.mark_address_as_gotoed(address)
        return label 

    def get_address_by_door_id(self, DoorId, RoutedF=False):
        address = self.__d2la[DoorId][0]
        if RoutedF:
            self.mark_address_as_routed(address)
        return address

    def get_door_id_by_address(self, Address):
        for door_id, info in self.__d2la.iteritems():
            if info[0] == Address: return door_id
        return None

    def get_label_by_address(self, Address, GotoedF=False):
        for address, label in self.__d2la.itervalues():
            if address == Address: 
                self.mark_address_as_gotoed(address)
                return label
        return None

    def get_door_id_by_label(self, Label):
        for door_id, info in self.__d2la.iteritems():
            if info[1] == Label: return door_id
        return None

    def get_address_by_label(self, Label):
        for door_id, info in self.__d2la.iteritems():
            if info[1] == Label: return info[0]
        return None

    def mark_address_as_gotoed(self, Address):
        self.__gotoed_address_set.add(Address)
        if False: # True/False switches debug output
             self.__debug_address_usage(Address, 18)

    def mark_label_as_gotoed(self, Label):
        self.mark_address_as_gotoed(self.get_address_by_label(Label))

    def mark_door_id_as_gotoed(self, DoorId):
        self.mark_address_as_gotoed(self.get_address_by_door_id(DoorId))

    def mark_address_as_routed(self, Address):
        self.__routed_address_set.add(Address)
        # Any address which is subject to routing is 'gotoed', at least inside
        # the router (e.g. "switch( ... ) ... case AdrX: goto LabelX; ...").
        self.mark_address_as_gotoed(Address)

    def mark_label_as_routed(self, Label):
        self.mark_address_as_routed(self.get_address_by_label(Label))

    def mark_door_id_as_routed(self, DoorId):
        self.mark_address_as_routed(self.get_address_by_door_id(DoorId))

    def map_incidence_id_to_state_index(self, IncidenceId):
        assert isinstance(IncidenceId, (int, long)) or IncidenceId in E_IncidenceIDs, \
               "Found <%s>" % IncidenceId

        index = self.__map_incidence_id_to_state_index.get(IncidenceId)
        if index is None:
            index = sm_index.get()
            self.__map_incidence_id_to_state_index[IncidenceId] = index

        return index
    
dial_db = DialDB()

class DoorID_Scheme(tuple):
    """A TargetByStateKey maps from a index, i.e. a state_key to a particular
       target (e.g. a DoorID). It is implemented as a tuple which can be 
       identified by the class 'TargetByStateKey'.
    """
    def __new__(self, DoorID_List):
        return tuple.__new__(self, DoorID_List)

    @staticmethod
    def concatinate(This, That):
        door_id_list = list(This)
        door_id_list.extend(list(That))
        return DoorID_Scheme(door_id_list)

__routed_address_set = set([])

class IfDoorIdReferencedCode:
    def __init__(self, DoorId, Code=None):
        """LabelType, LabelTypeArg --> used to access __address_db.

           Code  = Code that is to be generated, supposed that the 
                   label is actually referred.
                   (May be empty, so that that only the label is not printed.)
        """
        assert isinstance(Code, list) or Code is None

        self.label = dial_db.get_label_by_door_id(DoorId)
        if Code is None: self.code = [ self.label, ":" ]
        else:            self.code = Code

class IfDoorIdReferencedLabel(IfDoorIdReferencedCode):
    def __init__(self, DoorId):
        IfDoorIdReferencedCode.__init__(self, DoorId)

def get_plain_strings(txt_list):
    """-- Replaces unreferenced 'CodeIfLabelReferenced' objects by empty strings.
       -- Replaces integers by indentation, i.e. '1' = 4 spaces.
    """

    size = len(txt_list)
    i    = -1
    while i < size - 1:
        i += 1

        elm = txt_list[i]

        if type(elm) in [int, long]:    
            # Indentation: elm = number of indentations
            txt_list[i] = "    " * elm

        elif not isinstance(elm, IfDoorIdReferencedCode): 
            # Text is left as it is
            pass

        elif dial_db.label_is_gotoed(elm.label): 
            # If an address is referenced, the correspondent code is inserted.
            txt_list[i:i+1] = elm.code
            # print "#elm.code:", elm.code
            # txt_list = txt_list[:i] + elm.code + txt_list[i+1:]
            size += len(elm.code) - 1
            i    -= 1
        else:
            # If an address is not referenced, the it is replaced by an empty string
            txt_list[i] = ""

    return txt_list

def __nice(SM_ID): 
    assert isinstance(SM_ID, (long, int))
    return repr(SM_ID).replace("L", "").replace("'", "")
    
#__label_db = {
#    # Let's make one thing clear: addresses of labels are aligned with state indices:
#    "$entry":                 lambda DoorId:      __address_db.get_entry(DoorId),
#    # 
#    "$terminal":              lambda TerminalIdx: __address_db.get("TERMINAL_%s"        % __nice(TerminalIdx)),
#    "$terminal-router":       lambda NoThing:     __address_db.get("__TERMINAL_ROUTER"),
#    "$terminal-direct":       lambda TerminalIdx: __address_db.get("TERMINAL_%s_DIRECT" % __nice(TerminalIdx)),
#    "$terminal-general-bw":   lambda NoThing:     __address_db.get("TERMINAL_GENERAL_BACKWARD"),
#    "$terminal-EOF":          lambda NoThing:     __address_db.get("TERMINAL_END_OF_STREAM"),
#    "$terminal-FAILURE":      lambda SM_Id:       __address_db.get("TERMINAL_FAILURE_%s" % SM_Id),
#    #
#    "$state-router":          lambda NoThing:     __address_db.get("__STATE_ROUTER"),
#    #
#    "$reload":                lambda StateIdx:    __address_db.get("STATE_%s_RELOAD"    % __nice(StateIdx)),
#    "$reload-FORWARD":        lambda StateIdx:    __address_db.get("__RELOAD_FORWARD"),
#    "$reload-BACKWARD":       lambda StateIdx:    __address_db.get("__RELOAD_BACKWARD"),
#    "$drop-out":              lambda StateIdx:    __address_db.get("STATE_%s_DROP_OUT" % __nice(StateIdx)),
#    "$re-start":              lambda NoThing:     __address_db.get("__REENTRY_PREPARATION"),
#    "$re-start-2":            lambda NoThing:     __address_db.get("__REENTRY_PREPARATION_2"),
#    "$start":                 lambda NoThing:     __address_db.get("__REENTRY"),
#    "$skipper-reload":        lambda StateIdx:    __address_db.get("__SKIPPER_RELOAD_TERMINATED_%s" % __nice(StateIdx)),
#    "$bipd-return":           lambda DetectorID:  __address_db.get("BIPD_%i_RETURN" % DetectorID),
#    "$bipd-terminal":         lambda DetectorID:  __address_db.get("BIPD_%i_TERMINAL" % DetectorID),
#    # There may be more than one ... in skipp for example ...
#    "$init_state_transition_block": lambda StateIndex:   __address_db.get("INIT_STATE_%i_TRANSITION_BLOCK" % StateIndex),
#}
