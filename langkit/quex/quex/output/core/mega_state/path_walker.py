"""(C) Frank-Rene Schaefer

   Path Compression ___________________________________________________________

   Consider the file 'engine/analyzer/mega_state/path/core.py' for a detailed 
   explanation of path compression.

   Code Generation ____________________________________________________________

   Let 'path walker' be a code fragment that is able to 'walk' along a given
   path and follow a 'skeleton', i.e. a general transition map, if the current
   character is not the one of the path. As described in the above file, 
   a state is defined by a 'path walker' index and an iterator position that
   points to the position of a specific character string. Following code
   fragments need to be generated:

   (1) The pathes, i.e. array containing identified sequences, e.g.

        QUEX_CHARACTER_TYPE   path_0 = { 'o', 'r', PTC }; 
        QUEX_CHARACTER_TYPE   path_1 = { 'h', 'i', 'l', 'e', PTC }; 
        QUEX_CHARACTER_TYPE   path_2 = { 'e', 't', 'u', 'r', 'n', PTC }; 

       where PTC is the path terminating code that must be 
       different from the buffer limit code BLC.

       The init state shall usually not be a path state. It rather routes
       to paths. This is why identified pathes usually do not contain the
       first character of a related keyword. Note, however, that quex 
       may find paths that are not explicitly considered by the user.

   (2) The path walker.

       The path walker consist of a 'prolog' where the current input character
       is checked whether it belongs to the path, and the remainging trigger
       map, in case that the path is left, e.g.

         PATH_WALKER_1:
            /* Single Character Check */
            if   input == *path_iterator: ++path_iterator; goto PATH_WALKER_1
            elif *path_iterator == PTC:     goto STATE_3

            /* Common Transition Map */
            if   x < 'a': drop out
            elif x > 'z': drop out
            else:         goto STATE_4

   (3) State entries

       It is very plausible that states that belong to a path are not
       entered except through 'path walk' along the character sequence.
       In general, however, a state of a path might be entered from
       anywhere. Thus, at least for those states that are entered from
       elsewhere, a path entry must be provided. 

       A path entry consists of: setting the path iterator and goto the
       related path walker. Additionally, state attributes, such as 
       'acceptance' and 'store_input_position' must be considered. 
       Example:

          STATE_10:
                path_iterator = path_0;
                goto PATH_WALKER_1;
          STATE_11:
                path_iterator = path_0 + 1;
                goto PATH_WALKER_1;
          ...
            
    (4) State router, this might be necessary, if states are non-uniform.
        Because, after reload the current state entry must passed by again.
        In buffer based analysis no state router is required. Example of 
        a state router (same as for template compression):
        
        
            switch( state_index ) {
            case 2222: goto STATE_2222;
            case 3333: goto STATE_3333;
            ...
            }
"""
from quex.engine.analyzer.door_id_address_label        import dial_db
##from quex.output.core.state.core                import input_do
from quex.output.core.variable_db       import variable_db

from quex.blackboard import Lng

def framework(txt, PWState, TheAnalyzer):
    """Implement the Pathwalker's framework. The scheme for a path-walker
       is the following:

           Pathwalker Head:

              Compares the current 'input' character if it is still
              on the path or not. If it is on the path we increment
              the 'path_iterator' and re-enter the path walker. If
              not, then the thread of control enters the transition
              map.

           Pathwalker Transition Map:

             The transition map is the common transition map that all
             implemented states had in common. Now, transitions to 
             states outside the path may happen.
    """
    # Three Versions of PathWalker Heads:
    if PWState.uniform_door_id is not None:
        # UNIFORM PATHS: Along the path, always the same (or no) commands are executed.
        #
        # PathWalker Head Implementation:
        #
        #        if input == *path_iterator:
        #           path_iterator += 1
        #           if *path_iterator != TerminationCode: goto CommonPathWalkerDoor
        #           else:                                 goto TerminalDoor
        #
        # -- "goto CommonPathWalkerDoor"
        goto_next_door = "            %s\n" % Lng.GOTO(PWState.uniform_door_id)

        # -- "goto TerminalDoor"
        if PWState.uniform_terminal_door_id is not None:
            # All path have same terminal state and enter it at the same door
            goto_terminal_door   = "            %s\n" % Lng.GOTO(PWState.uniform_terminal_door_id)
        else:
            # The terminals of the paths are different
            # 
            # The "goto TerminalDoor" is implemented for each path. The single
            # goto is split into a sequence:
            #
            #      if      path_iterator == path_0_end:  goto TerminalDoorOfPath0
            #      else if path_iterator == path_1_end:  goto TerminalDoorOfPath1
            #      else if path_iterator == path_2_end:  goto TerminalDoorOfPath2
            #      ...
            tmp    = ""
            offset = 0
            for path_id, door_id_sequence in enumerate(PWState.door_id_sequence_list):
                offset += len(door_id_sequence) + 1
                tmp +=  "            %s"  % Lng.IF("path_iterator", "==", "&path_walker_%i_path_base[%s]" %  \
                                                          (PWState.index, offset - 1), FirstF=(path_id == 0))                                  \
                      + "                %s\n" % Lng.GOTO(door_id_sequence[-1]) 
            tmp += "            %s"       % Lng.ELSE                                  
            tmp += "                %s\n" % Lng.UNREACHABLE
            tmp += "            %s\n"     % Lng.END_IF()                                  
            goto_terminal_door = tmp

        path_walker_head = \
            ["    %s"            % Lng.IF_INPUT("==", "*path_iterator"),
             "        %s\n"      % Lng.PATH_ITERATOR_INCREMENT,
             "        %s"        % Lng.IF("*path_iterator", "!=", "QUEX_SETTING_PATH_TERMINATION_CODE"),
             goto_next_door,
             "        %s"        % Lng.ELSE,                                  
             goto_terminal_door,
             "        %s\n"      % Lng.END_IF(),
             "    %s\n"          % Lng.END_IF()]
    else:
        # NON UNIFORM PATHS
        #
        # PathWalker Head Implementation:
        #
        #     if input == *path_iterator:
        #        path_iterator += 1
        #        goto NextDoor(path_iterator)
        #
        # Here, the "goto TerminalDoor" results from NextDoor(path_iterator)
        # automatically, when the path_iterator stands on the last element.
        #
        label          = "path_walker_%i_state_base[path_iterator - path_walker_%i_reference]" \
                         % (PWState.index, PWState.index)
        goto_next_door = "%s" % (Lng.GOTO_BY_VARIABLE(label))

        path_walker_head = ["    %s"       % Lng.IF_INPUT("==", "*path_iterator"),
                            "        %s\n" % Lng.PATH_ITERATOR_INCREMENT,
                            "        %s\n" % goto_next_door,
                            "    %s\n"     % Lng.END_IF()]

    txt.extend(path_walker_head)
    return

def require_data(PWState, TheAnalyzer):
    """Defines the transition targets for each involved state.
    """
    
    variable_db.require("path_iterator")

    def __door_adr_sequences(PWState):
        result = ["{\n"]
        length = 0
        for path_id, door_id_sequence in enumerate(PWState.door_id_sequence_list):
            # NOTE: For all states in the path the 'from_state_index, to_state_index' can
            #       be determined, **except** for the FIRST state in the path. Thus for
            #       this state the 'door' cannot be determined in case that it is 
            #       "not uniform_doors_f()". 
            #
            #       However, the only occasion where the FIRST state in the path may be 
            #       used is reload during the FIRST state. The reload adapts the positions
            #       and acceptances are not changed. So, we can use the common entry
            #       to the first state as a reference here.
            ## print "#DoorID, Adr:", [(door_id, Lng.ADDRESS_BY_DOOR_ID(door_id)) for door_id in door_id_sequence]
            result.append("        ")
            result.append("/* Padding */0x0, ")
            result.extend("QUEX_LABEL(%i), " % dial_db.get_address_by_door_id(door_id, RoutedF=True)
                          for door_id in door_id_sequence)
            result.append("\n")

            length += len(door_id_sequence) + 1 # 1 padding element

        result.append("    }");
        return length, result

    def __character_sequences(PathList):
        result = ["{\n"]
        offset = 0
        for path_id, step_list in enumerate(PathList):
            ## Commenting the transition sequence is not dangerous. 'COMMENT' eliminates
            ## comment delimiters if they would appear in the sequence_str.
            ##   sequence_str = imap(lambda x: Interval(x[1]).get_utf8_string(), step_list[:-1])
            ##   memory.append(Lng.COMMENT("".join(sequence_str)) + "\n")

            result.append("        ")
            result.extend("%i, " % x.trigger for x in step_list[:-1])
            result.append("QUEX_SETTING_PATH_TERMINATION_CODE,")
            result.append("\n")

            offset += len(step_list)

        result.append("    }")
        return offset, result

    # (*) Path Walker Basis
    # The 'base' must be defined before all --> PriorityF (see table in variable_db)
    element_n, character_sequence_str = __character_sequences(PWState.path_list)

    offset = 0
    for path_id, step_list in enumerate(PWState.path_list):
        variable_db.require("path_walker_%i_path_%i", 
                            Initial = "&path_walker_%i_path_base[%i]" % (PWState.index, offset), 
                            Index   = (PWState.index, path_id))
        offset += len(step_list)

    variable_db.require_array("path_walker_%i_path_base", 
                              ElementN = element_n,
                              Initial  = character_sequence_str,
                              Index    = PWState.index)
    
    # (*) The State Information for each path step
    if PWState.uniform_door_id is None:
        element_n, door_adr_sequence_str = __door_adr_sequences(PWState)
        variable_db.require_array("path_walker_%i_state_base", 
                                  ElementN = element_n,
                                  Initial  = door_adr_sequence_str,
                                  Index    = PWState.index)

        # The path_iterator is incremented before the 'goto', thus
        # 'path_iterator - (path_base + 1)' gives actually the correct offset.
        # We define a variable for that, for elegance.
        variable_db.require("path_walker_%i_reference", 
                            Initial = "path_walker_%i_path_base" % PWState.index, 
                            Index   = (PWState.index))

