"""A state can be entered from multiple states. The actions performed upon
entry may differ, and partly be the same. A 'CommandTree' organizes command 
lists efficiently considering similarities. This module codes a command tree 
where its leafs are the entries from other states.

                from X               from Y             from Z
                   |                    |                  |
                   |                    |                  |   
             .-------------.      .-------------.    .-------------.
             |DoorID:      |      |DoorID:      |    |DoorID:      |
             | OpList |      | OpList |    | OpList |
             '-------------'      '-------------'    '-------------'
               parent \             / parent               / parent
                       \           /                      /
                      .-------------.                    /
                      |DoorID:      |                   /
                      | OpList |                  /
                      '-------------'                 /
                                   \ parent          /
                                    \               /
                                  .-------------------.
                                  | Root-DoorID       |
                                  |   TransitionMap   |
                                     ...

(C) Frank-Rene Schaefer
_______________________________________________________________________________
"""
from   quex.engine.analyzer.door_id_address_label import IfDoorIdReferencedLabel
from   quex.engine.operations.tree         import CommandTree
from   quex.engine.misc.tools                          import flatten_list_of_lists, \
                                                         typed
from   quex.blackboard                            import Lng

@typed(FirstF=bool)
def do(TheState):
    """Generate code for the entry into a state from multiple different states.

    RETURNS: [0] Code to be 'pasted' IN FRONT of the transition map
             [1] Code that can be 'pasted' anywhere. 

    The case where [1] is not None, is actually a special case. It is the case
    of the global entry into the state machine / analyzer function. 

    The actions (OpLists) to be executed may differ depending from where
    the state is entered. On the other hand, there may be shared commands in
    between the doors. To take profit from similarities, command lists are
    organized in a command tree as explained in the entry of the file.
    """
    cmd_tree = CommandTree.from_AnalyzerState(TheState)

    # Determin the branch that starts from a leaf and ends at the entry
    # of the transition map (Processor main body).
    straight_branch_door_id, \
    global_entry_f           = __select_the_straight(cmd_tree, TheState)

    done_set = set()
    pre_txt  = do_from_leaf_to_root(TheState, cmd_tree, straight_branch_door_id, 
                                    done_set, GlobalEntryF=global_entry_f)

    post_txt = do_leafs(TheState, cmd_tree, done_set)

    return pre_txt, post_txt

def __select_the_straight(OpTree, TheState):
    """One branch from leaf to root is special: It implements ALL nodes
    including the ROOT node. This branch has to be placed IMMEDIATELY before the
    transition map. All other branches are moveable inside the function.

    RETURNS: DoorID of selected leaf node.
    """
    # (*) Is there a 'global' entry?
    global_entry_door_id = TheState.entry.get_state_machine_entry_door_id()
    if global_entry_door_id is not None:
        return global_entry_door_id, True

    # (*) Otherwise, take the longest path to the root
    door_id = max(TheState.entry.door_id_set(), 
                  key=lambda door_id: OpTree.get_step_n_to_root(door_id))

    return door_id, False

def do_leafs(TheState, OpTree, done_set):
    """Create code starting from the 'leafs' of the command tree. The leafs are 
    the entry points from other states, i.e. the 'doors'.

    RETURNS: List of strings.
    """
    outer_door_id_set = TheState.entry.door_id_set()

    txt_list = []
    for door_id in outer_door_id_set:
        if door_id in done_set: continue
        branch_txt = do_from_leaf_to_root(TheState, OpTree, door_id, done_set)
        txt_list.append(branch_txt)

    # Flatten the list of lists, where the longest list has to come last.
    result = flatten_list_of_lists(sorted(txt_list, key=lambda x: len(x)))
    return result

def do_from_leaf_to_root(TheState, OpTree, LeafDoorId, done_set, GlobalEntryF=False):
    """Code the sequence from a leaf of the command tree to its root. This
    avoids unnecessary gotos from outer nodes to their parents. It stops,
    whenever a parent is already implemented.  Then, the function 'code()'
    automatically inserts a 'goto parent' at the end of the node.

    RETURNS: list of strings 
    
    The list of string implements nodes from a command tree leaf over all of
    its parents to the root, or the first already implemented parent.
    """
    txt = []
    if not GlobalEntryF:
        # When the entry is a global entry into the analyzer, then it is slipped
        # into at function begin. => no 'assert unreachable'! Else, yes!
        txt.append("\n\n    %s\n" % Lng.UNREACHABLE)

    txt.extend( 
        flatten_list_of_lists(
            __code(node, TheState, done_set, GlobalEntryF)
            for node in OpTree.iterable_to_root(LeafDoorId, done_set)
        )
    )
    return txt

def __code(Node, TheState, done_set, GlobalEntryF):
    """Code a node of the command tree in a sequence of nodes from leaf to
    root, parent by parent. As long as the parent is in the list, no goto is
    required. If the parent is implemented already, an explicit goto is 
    implemented.

    RETURNS: list of strings = code for node.
    """
    done_set.add(Node.door_id)

    txt = []
    __label_node(txt, Node)
    __comment(txt, Node, TheState, GlobalEntryF)

    # (*) The code of commands of the node
    txt.extend(
        Lng.COMMAND_LIST(Node.command_list)
    )

    # (*) The 'goto parent'.
    if Node.parent is None:
        # (*) AFTER: COMMAND_LIST -- The state debug info.
        txt.append(Lng.STATE_DEBUG_INFO(TheState, GlobalEntryF))
        # This is the root. 
        return txt

    elif Node.parent.door_id not in done_set:
        # As long as the parent is not done, it will be implemented immediately
        # after this node--no goto is required.
        return txt

    elif Node.command_list and Node.command_list[-1].is_conditionless_goto(): 
        # Goto is futile if the last command is an unconditional goto.
        return txt

    # Append the 'goto parent'
    txt.append("    %s\n" % Lng.GOTO(Node.parent.door_id))
    return txt

def __comment(txt, Node, TheState, GlobalEntryF):
    """Comment on the states from where the door is entered.

    RETURNS: String 

    The string contains information about the entries which the node represents.
    In MegaStates it may be possible that an entry representes entries TO 
    multiple different states.
    """
    # 'Node.child_set is None' => leaf node, i.e. entry from other state. 
    # Otherwise: nothing to be done.
    if Node.child_set: return "\n"

    transition_id_list = TheState.entry.get_transition_id_list(Node.door_id)
    if not transition_id_list: return "\n"

    msg = "".join(
        "(%s from %s) " % (x.target_state_index, x.source_state_index) \
        for x in transition_id_list
    )
    txt.append("    %s\n" % Lng.COMMENT(msg)[:-1])

def __label_node(txt, Node):
    """The 'label' of a node in the command tree. Note, that depending on child 
    number:

             == 0   => 'outer node' and gotoed from outside 
                    => label REQUIRED.
                    
             == 1   => 'inside node' and slipped in from child 
                    => no label required.
                    
             >= 2   => 'inside node' and gotoed from inside.
                    => label REQUIRED

    NOTE Case '>= 2': the 'goto parent' might be disabled by a 'goto'
         in the command list of a node. Consequently, it is possible that it is
         not gotoed, and therefore its implementation must be made CONDITIONAL. 

    NOTE Case == 0: There is a special case at the global entry into the
         analyzer. Then, no label is required. So, its safe to make it also
         CONDITIONAL.
    """
    if not Node.child_set:                   # leaf node (entered from outside!)
        txt.append(IfDoorIdReferencedLabel(Node.door_id))
    elif len(Node.child_set) >= 2:           # inner node (gotoed by child)
        txt.append(IfDoorIdReferencedLabel(Node.door_id))
    txt.append("\n")
