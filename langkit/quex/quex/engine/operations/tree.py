"""The 'Command Tree' __________________________________________________________

DEFINITION: 
    
    A 'Door' is associated with a specific command list which is executed upon 
    entry into a states. That is, a door from the door's id the command list can
    be concluded and vice versa:

                      DoorID <--> Commands to be executed.

PRINCIPLE:

Several doors into a state may share commands which can be extracted into a
tail. Imagine Door 1 and 2 in the following example:
    
            Door 1:  [ X, Y, Z ]

            Door 2:  [ U, V, Y, Z ]

Both share the Y and Z and the end. So, a common tail can be implemented and
the configuration becomes:
    
            Door 1:  [ X ] ------.
                                 +----> [ Y, Z ]
            Door 2:  [ U, V ] ---'

in which case Y and Z are implemented only once, instead of each time for door
1 and door 2.

_______________________________________________________________________________

PROCEDURE:

A state's entry already determined the unique set of command lists and
associated each command list with a DoorID. Starting from there, a set 
of Door-s can be built which are all connected to the 'root', i.e. the 
final entry into the state.

            Door 1:  [ X, Y, Z ] --------.
            Door 2:  [ U, V, Y, Z ] ------+
            ...                            +----> root
            Door N:  [ P, Q, Z ] ---------'

Now, build a matrix that tells what doors have what tail in common.

                   1   2  ...  N         (Note, only one triangle 
                1                         of the matrix needs to be
                2                         determined; symmetrie!)
               ...       T(i,k)
                N

Let T(i,k) be the common tail of Door 'i' with Door 'k'. Considering
the longest tail of the whole matrix. Then it is save to assume that

       There is no way that more commands can be cut out of 'i' 
       and 'k' then with the given combination. 

Thus, once the combination is done, 'i' and 'k' is done and no longer
subject to combination considerations. The combination results in 
a new door. Let's assume i was 1 and k was 2:

            Door 1:  [ X ] -------.   Door x0
            Door 2:  [ U, V ] -----+--[ Y, Z ]--.
            ...                                  +----> root
            Door N:  [ P, Q, Z ] ---------------'

The new Door x0 is the 'parent' of Door 1 and Door 2. Its parent is root. 
Now, 1 and 2 are done and what remains is Door x0. 

Note, only those doors can combine their 'tails' whose parent is the same.
The parent represents the 'tail' commands. With the current algorithm, though,
all generated nodes have 'root' as their parent. Thus, the requirement that
all candidates share the parent is given.

_______________________________________________________________________________

FUNCTIONS:

    do(): -- Does the optimization. Returns the root door. Iterate over
             tree by each node's child_set.
           
(C) Frank-Rene Schaefer
_______________________________________________________________________________
"""
from   quex.engine.analyzer.door_id_address_label import DoorID, dial_db
import quex.engine.operations.shared_tail  as     shared_tail
from   quex.engine.misc.tools                          import flatten_list_of_lists

from quex.engine.misc.tools import typed, TypedDict

from operator         import attrgetter

class CommandTree:
    def __init__(self, StateIndex, DoorId_OpList_Iterable):
        """StateIndex -- Index of state for which one operates.
                         (needed for new DoorID generation)
           
           DoorId_OpList_Iterable -- Iterable over pairs of 

                         (DoorID, command lists)

        NOTE: The command lists are MODIFIED during the process of finding
              a command tree!
        """
        shared_tail_db = SharedTailDB(StateIndex, DoorId_OpList_Iterable)

        while shared_tail_db.pop_best():
            pass

        self.shared_tail_db = shared_tail_db
        self.root    = shared_tail_db.root
        self.door_db = shared_tail_db.door_db

    @staticmethod
    def from_AnalyzerState(TheState):
        door_id_command_list = [
            (ta.door_id, ta.command_list) 
            for ta in TheState.entry.itervalues()
        ]
        return CommandTree(TheState.index, door_id_command_list)

    def iterable_to_root(self, DoorId, done_set=None):
        """Iterate from a node, parent by parent, to the root of the tree.

        If 'done_set' is specified, it is taken care that:
            (1) No Node with Node.door_id from DoneSet is yielded
            (2) The DoorID of yielded nodes is added to done_set

        YIELDS: Door which is a node in the tree.
        """
        node = self.door_db.get(DoorId)
        if done_set is None:
            while node is not None:
                yield node
                node = node.parent
        else:
            while node is not None:
                if node.door_id not in done_set: yield node
                done_set.add(node.door_id)
                node = node.parent

    def get_step_n_to_root(self, LeafDoorId):
        return sum(1 for x in self.iterable_to_root(LeafDoorId))

    def get_string(self, CommandAliasDb=None):
        def cmd_iterable(DoorDb):
            for door in self.door_db.itervalues():
                for cmd in door.command_list:
                    yield cmd

        if CommandAliasDb is None:
            CommandAliasDb = {}
            i              = 0
            for cmd in cmd_iterable(self.door_db):
                if cmd in CommandAliasDb: continue
                CommandAliasDb[cmd] = "%X" % i
                i += 1

        return self.shared_tail_db.get_string(CommandAliasDb)

class Door(object):
    __slots__ = ("door_id", "command_list", "parent", "child_set")
    def __init__(self, DoorId, OpList, Parent, ChildSet):
        self.door_id      = DoorId
        self.command_list = OpList
        self.parent       = Parent
        self.child_set    = ChildSet

class SharedTailCandidateInfo(object):
    """A SharedTailCandidateInfo is 1:1 associated with a shared tail command list.
    It contains the DoorID-s of doors that share the tail and what indices would
    have to be cut out of the door's command list if the shared tail was to be 
    extracted. All this information is stored in a map:

                           DoorId --> Cut Index List

    The shared tail which is related to the SharedTailCandidateInfo is NOT stored here.
    It is stored inside the SharedTailDB as a key.
    """
    __slots__ = ("cut_db", "shared_tail", "tail_length")
    def __init__(self, SharedTail, DoorId_A, CutIndicesA, DoorId_B, CutIndicesB):
        self.cut_db = {
            DoorId_A: CutIndicesA,
            DoorId_B: CutIndicesB,
        }
        self.shared_tail = SharedTail
        self.tail_length = len(SharedTail)

    def add(self, DoorId, CutIndexList):
        """DoorId       -- DoorID to be added.
           CutIndexList -- List of indices to be cut in order to extract
                           the shared tail.
        """
        entry = self.cut_db.get(DoorId)
        if entry is not None:
            assert entry == CutIndexList
            return
        self.cut_db[DoorId] = CutIndexList

    def door_id_iterable(self):
        return self.cut_db.iterkeys()

    def remove_door_id(self, DoorId):
        """Deletes the DoorId from the list of sharing DoorID-s.

        RETURNS: True  -- if there are still more than 1 sharer.
                          (1 sharer has none to share with).
                 False -- If there are less than 2 sharers. The 
                          candidate can now be dropped.
        """
        if DoorId in self.cut_db: del self.cut_db[DoorId]
        return len(self.cut_db) > 1

    def __str__(self):
        txt = [ 
            "    .tail_length: %i;\n" % self.tail_length,
            "    .cut_db: {\n",
        ]
        for door_id, cut_indices in self.cut_db.iteritems():
            txt.append("        %s -> { %s }\n" % (str(door_id), "".join("%i, " % i for i in cut_indices)))
        txt.append("    }\n")
        return "".join(txt)

class SharedTailDB:
    """_________________________________________________________________________                     
    Database that allows to combine shared tails of command list sequentially.

       .pop_best() -- Find the best combination of Door-s who can share a 
                      command list tail. 
                   -- Extract the tail from the Door-s' command lists. 
                   -- Generate a new Door with the tail. 
                      + The 'parent' the Door-s' parent (i.e. always '.root'). 
                      + The childs are the Door-s from which the tail has been 
                        extracted.
                   -- Delete the sharing Door-s from the list of candidates.
                   -- Determine sharing relationships of new Door with present
                      candidates.

    The '.pop_best()' function is to be called repeatedly until it returns
    'None'. In that case there are no further candidates for shared tail 
    combinations.
    ____________________________________________________________________________
    MEMBERS:

        .state_index = Index of the state for which the procedure operates.
                       This is required for the generation of new DoorID-s.

        ._candidate_db     = Dictionary containing all Door-s which are opt to 
                       share a command list tail.

                         map:    DoorID --> Door

        ._tail_db          = Dictionary holding information about what shared
                       tail (as a tuple) is shared by what Door-s.

                         map:    shared tail --> SharedTailCandidateInfo
    ____________________________________________________________________________                     
    """
    __slots__ = ("state_index", "door_id_set", "db")

    @typed(DoorId_OpList=[tuple])
    def __init__(self, StateIndex, DoorId_OpList):
        self.state_index = StateIndex
        root_child_set   = set(door_id for door_id, cl in DoorId_OpList) 
        self.root        = Door(dial_db.new_door_id(StateIndex), [], None,
                                root_child_set)
        self.door_db     = TypedDict(DoorID, Door) # {}   # map: DoorID --> Door 
        #                       #  ... with ALL Door-s related to the 'problem'

        # map: Shared Tail --> SharedTailCandidateInfo
        self._tail_db      = {} # map: command list tail --> those who share it
        self._candidate_db = {} # map: DoorID --> Door 
        #                       #  ... but only with those DoorID-s that are 
        #                       #      subject to shared tail investigation.

        # Doors which cannot extract a tail are dropped from consideration. 
        # (See discussion at end [DROP_NON_SHARER])
        # BUT: First the WHOLE SET of Door-s must be considered! 
        # If a Door shares from the beginning => into 'good_set'.
        # Else, look into _tail_db if it finally shared something. 
        good_set = set()

        for door_id, command_list in DoorId_OpList:
            door = Door(door_id, command_list, self.root, set())
            if self._tail_db_enter(door_id, door): 
                good_set.add(door_id)
            self._candidate_db[door_id] = door
            self.door_db[door_id]       = door

        # Drop non-sharing Door-s. 
        # (See [DROP_NON_SHARER])
        for door_id in self._candidate_db.keys():
            if   door_id in good_set:                continue
            elif self._tail_db_has_door_id(door_id): continue
            del self._candidate_db[door_id]

    def pop_best(self):
        """(1) Determine the best shared tail be combined into a new node.
           (2) Contruct a new node implementing the shared tail.
           (3) Update internal databases, so the combined Door-s are
               no longer considered.
        
        RETURNS: True -- if it was possible to combine two Doors and 
                         extract a shared tail of comands.
                 False -- if it was not possible.
        """
        if len(self._tail_db) == 0:
            return False

        best_candidate = self._find_best()
        self._setup_new_node(best_candidate)

        return True

    def _find_best(self):
        """Find the combination that provides the most profit. That is, the
        combination which is shared by the most and has the longest tail.
        """
        assert len(self._tail_db) > 0

        best_value     = -1
        best_candidate = None
        for tail, candidate in self._tail_db.iteritems():
            # Combine a maximum number of operations --> consider only tail length
            value = candidate.tail_length
            if value <= best_value: continue
            best_value     = value
            best_candidate = candidate

        # The best_candidate cannot be None if len(self._tail_db) > 0
        assert best_candidate is not None
        return best_candidate

    def _remove(self, DoorId):
        """Remove all references of DoorId inside the database. That is, the
        related door does no longer act as candidate for further shared-tail
        combinations.
        """
        trash = []
        for tail, candidate in self._tail_db.iteritems():
            if candidate.remove_door_id(DoorId) == True: continue
            # candidate is now empty --> no shared tail
            trash.append(tail)

        for tail in trash:
            del self._tail_db[tail]

        del self._candidate_db[DoorId]

    def _setup_new_node(self, Candidate):
        """A Tail has been identified as being shared and is now to be extracted
        from the sharing doors. Example: 'Y, Z' is a shared tail of doord 1 and 2:
        
                        Door 1:  [ X, Y, Z ] ---------.
                                                      |
                        Door 2:  [ U, V, Y, Z ] ------+
                        ...                            +----> root
                        Door N:  [ P, Q, Z ] ---------'

        The 'Y, Z' is extracted into a new door, and door 1 and 2 need to goto the
        new door after the end of their pruned tail.

                        Door 1:  [ X ] --------.  new Door 
                                               |
                        Door 2:  [ U, V ] -----+--[ Y, Z ]--.
                        ...                                  +----> root
                        Door N:  [ P, Q, Z ] ---------------'

        PROCEDURE: (1) Generate DoorID for the new node.
                   (2) Prune the command lists of the sharing doors.
                   (3) Set their 'parent' to the new node's DoorID.
                   (4) Enter new node with (DoorID, Tail) into the door database.

        RESULT: The new Door

            .command_list = the shared tail
            .child_set    = all sharing doors (Door 0, Door 1, ...)
            .parent       = root (which is ALWAYS the parent of nodes subject 
                            to investigation).
        """
        new_door_id = dial_db.new_door_id(self.state_index)
        child_set   = set(Candidate.door_id_iterable())
        new_door    = Door(new_door_id, list(Candidate.shared_tail), self.root, child_set)

        self.door_db[new_door_id] = new_door

        for door_id, cut_index_list in Candidate.cut_db.items(): # NOT: 'iteritems()'
            door = self._candidate_db[door_id]
            # The cut_index_list MUST be sorted in a way, that last index comes
            # first. Otherwise, the 'del' operator cannot be applied conveniently. 
            last_i = None
            for i in cut_index_list:
                assert last_i is None or i < last_i
                del door.command_list[i]
                last_i = i

            # Doors that have been combined, did so with the 'longest' possible
            # tail. Thus, they are done!
            self._remove(door_id)
            # -- '.parent' is only set to new doors.
            # -- all new doors relate to '.root'.
            # => The parent of all considered Door-s in the database is '.root'
            assert door.parent == self.root
            door.parent = new_door
            self.root.child_set.remove(door_id)

        self.root.child_set.add(new_door_id)

        if self._tail_db_enter(new_door_id, new_door):
            # A new Door that does not share anything does not have to be 
            # considered. See discussion at end of file [DROP_NON_SHARER].
            self._candidate_db[new_door_id] = new_door

    def _tail_db_enter(self, NewDoorId, NewDoor):
        """(1) Determine the shared tail with any other available Door.
           (2) Enter the new Door in door_db.

        RETURNS: True  -- If the Door was able to extract at least one shared tail.
                 False -- If not.
        """

        shared_f = False
        for door_id, door in self._candidate_db.iteritems():
            tail,   \
            x_cut_indices, y_cut_indices = shared_tail.get(NewDoor.command_list, 
                                                           door.command_list)
            if tail is None: continue
            shared_f = True

            self._tail_db_register(tail, NewDoorId, door_id, 
                                   x_cut_indices, y_cut_indices)

        return shared_f

    def _tail_db_register(self, SharedTail, DoorId_A, DoorId_B, CutIndicesA, CutIndicesB):
        """Registers the 'SharedTail' in _tail_db as being shared by DoorId_A, And DoorId_B.
        """
        entry = self._tail_db.get(SharedTail)
        # entry = Dictionary that maps from 
        if entry is None:
            entry = SharedTailCandidateInfo(SharedTail, 
                                            DoorId_A, CutIndicesA, 
                                            DoorId_B, CutIndicesB)
            self._tail_db[SharedTail] = entry
        else:
            entry.add(DoorId_A, CutIndicesA)
            entry.add(DoorId_B, CutIndicesB)

    def _tail_db_has_door_id(self, DoorId):
        """RETURNS: True  -- if any shared tail candidate contains the DoorId
                             as a sharer.
                    False -- if no candidate contains the DoorId as sharer.
        """
        for candidate in self._tail_db.itervalues():
            if DoorId in candidate.cut_db: return True
        return False

    def get_string(self, CommandAliasDb):
        txt = [
            ".state_index:    %i;\n" % self.state_index,
            ".root:           door_id: %s; child_n: %i\n" % (str(self.root.door_id), len(self.root.child_set)),
            ".candidate_db.keys(): %s;\n" % "".join("%s, " % str(door_id) for door_id in self._candidate_db.iterkeys()),
            ".shared_tails: {\n"
        ]
        
        for tail, candidate_set in sorted(self._tail_db.iteritems()):
            txt.append("  (%s) -> {\n" % ("".join("%s " % CommandAliasDb[cmd] for cmd in tail)).strip())
            txt.append(str(candidate_set))
        txt.append("  }\n")

        return "".join(txt)

    def get_tree_text(self, CommandAliasDb, Node=None, Depth=0):
        """__dive: indicate recursion. May be solved by 'TreeWalker'.
        """
        if Node is None: 
            Node = self.root

        txt = flatten_list_of_lists(
            self.get_tree_text(CommandAliasDb, self.door_db[door_id], Depth+1)
            for door_id in sorted(Node.child_set)
        )

        txt.extend([
            "    " * (Depth + 1), 
            ".--", 
            str(Node.door_id), 
            " [%s]\n" % ("".join("%s " % CommandAliasDb[cmd] for cmd in Node.command_list)).strip()
        ])
        return txt

def get_string_DELETED(DoorTreeRoot):
    """ActionDB can be received, for example from the 'entry' object.
       If it is 'None', then no transition-id information is printed.
    """
    ActionDB = None
    OnlyFromStateIndexF = None
    dtr = DoorTreeRoot
    def door_id_to_transition_id_list(DoorId, ActionDB):
        if ActionDB is None:
            return None
        return [
            transition_id for transition_id, action in ActionDB.iteritems()
                          if action.door_id == DoorId
        ]

    txt = []
    if dtr.child_set is not None:
        def sort_key(X, ActionDB):
            return (door_id_to_transition_id_list(X.door_id, ActionDB), X)
                
        for child in sorted(dtr.child_set, key=lambda x: sort_key(x, ActionDB)):
            txt.append("%s\n" % child.get_string(ActionDB))

    if dtr.door_id is not None: 
        txt.append("[%s:%s]: " % (dtr.door_id.state_index, dtr.door_id.door_index))
    else:                        
        txt.append("[None]: ")

    if ActionDB is not None:
        transition_id_list = door_id_to_transition_id_list(dtr.door_id, ActionDB)
       
        for transition_id in sorted(transition_id_list, key=attrgetter("target_state_index", "source_state_index")):
            if OnlyFromStateIndexF:
                txt.append("(%s) " % transition_id.source_state_index)
            else:
                txt.append("(%s<-%s) " % (transition_id.target_state_index, transition_id.source_state_index))

    if dtr.command_list is not None:
        txt.append("\n")
        for cmd in dtr.command_list:
            cmd_str = str(cmd)
            cmd_str = "    " + cmd_str.replace("\n", "\n    ") + "\n"
            txt.append(cmd_str)
        if len(txt[-1]) == 0 or txt[-1][-1] != "\n":
            txt.append("\n")
    else:
        txt.append("\n")

    if dtr.parent is None: txt.append("    parent: [None]\n")
    else:                  txt.append("    parent: [%s:%s]\n" % \
                                       (str(dtr.parent.door_id.state_index), 
                                        str(dtr.parent.door_id.door_index)))
    return "".join(txt)





"""[DROP_NON_SHARER]___________________________________________________________

SCENARIO:

   A Door has been identified as not having an extractable shared tail with any
   other Door. 
   
QUESTION:
    
   Can there be a possibility later for the Door to extract a shared tail?

CASES:

   (0) Door does not share any command with another door.
       => IMPOSSIBLE to share later.

   (1) Door has a command A = Cl[i] in common with another Door, but it cannot 
       be moved to the tail, because there is a command X = Cl[k] with k > i 
       that has interferring register access.

            The Door's command list: [ ... A ... X ... ]
            The Counterpart:         [ ... A ... ]

       -- If X could be shared by anyone then it would appear in the shared tail.
          => This is not our scenario as described above.

       => X is not shared by anyone and will remain blocking the 'move to tail'.
       => IMPOSSIBLE to share later.

   (2) Door shares a command A but the counter part has a blocking command that
       hinders it from moving A to the tail. The counterpart needs to find a third 
       Door with whom it could share 'X'.

            Door:        [ ... A ... ] ----------.
                                                 |
            Counterpart: [ ... A ... X ... ] ----+----> root
                                                 |
            Third:       [ ... X ... ] ----------'

       Then, however, the Door could not share a tail anymore with the counter
       part. CLEARLY: If Door cannot jump to [ X ... ] otherwise it would 
       execute a command list which is different from its original.
      
            Door:        [ ... A ... ] ---------------------.
                                                            |
            Counterpart: [ ... A ... ] ---+---[ X ... ] ----+----> root
                                          |
            Third:       [ ... X ... ] ---'

       => IMPOSSIBLE to share later.

CONCLUSION:

    If after the initialization process has completed, a Door has shown not
    to be able to extract a shared tail with any other Door, then this Door
    is not able to share later. It can be dropped from consideration of the
    shared-tail finder algorithm.
_______________________________________________________________________________
"""

