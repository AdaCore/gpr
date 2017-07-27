from   quex.engine.analyzer.mega_state.template.state    import TemplateState
from   quex.engine.analyzer.mega_state.path_walker.state import PathWalkerState
from   quex.output.core.state.transition_map.code       import MegaState_relate_to_transition_code
import quex.output.core.state.transition_map.core  as transition_block
import quex.output.core.state.entry            as     entry_coder
import quex.output.core.mega_state.template    as     template
import quex.output.core.mega_state.path_walker as     path_walker

class Handler:
    def __init__(self, TheState):
        if isinstance(TheState, PathWalkerState):
            self.require_data = path_walker.require_data
            self.framework    = path_walker.framework   
            self.state_key_str      = "path_iterator - path_walker_%s_path_base" % TheState.index 
            self.debug_drop_out_str = "__quex_debug_path_walker_drop_out(%i, path_walker_%s_path_base, path_iterator);\n" \
                                      % (TheState.index, TheState.index)
        elif isinstance(TheState, TemplateState):
            self.require_data = template.require_data
            self.framework    = template.framework   
            self.state_key_str      = "state_key"
            self.debug_drop_out_str = "__quex_debug_template_drop_out(%i, state_key);\n" % TheState.index
        else:
            assert False

        self.state = TheState

    def debug_info_map_state_key_to_state_index(self, txt):
        txt.append("#   define __QUEX_DEBUG_MAP_STATE_KEY_TO_STATE(X) ( \\\n")
        pair_list = [
            (state_key, state_index)
            for state_key, state_index in self.state.ski_db.iterable_state_key_state_index_pairs()
        ]
        pair_list.sort()
        for state_key, state_index in pair_list[:-1]:
            txt.append("             (X) == %i ? %i :    \\\n" % (state_key, state_index))

        state_key, state_index = pair_list[-1]
        txt.append("             (X) == %i ? %i : 0)" % (state_key, state_index))

        if isinstance(self.state, PathWalkerState):
            txt.append("\n#   define __QUEX_DEBUG_MAP_PATH_BASE_TO_PATH_ID(PB) ( \\\n")
            for path_id in xrange(len(self.state.path_list) - 1):
                txt.append("             (PB) == path_walker_%i_path_%i ? %i :    \\\n" \
                           % (self.state.index, path_id, path_id))
            path_id = len(self.state.path_list) - 1
            txt.append("             (PB) == path_walker_%i_path_%i ? %i : 0)" \
                       % (self.state.index, path_id, path_id))

    def debug_info_undo_map_state_key_to_state_index(self, txt):
        txt.append("\n#   undef __QUEX_DEBUG_MAP_STATE_KEY_TO_STATE\n")
        if isinstance(self.state, PathWalkerState):
            txt.append("#   undef __QUEX_DEBUG_MAP_PATH_BASE_TO_PATH_ID\n")

def do(txt, TheState, TheAnalyzer):
    specific = Handler(TheState)

    # [X] Helper definitions
    specific.debug_info_map_state_key_to_state_index(txt)

    # (*) Entry _______________________________________________________________
    pre_txt, post_txt = entry_coder.do(TheState) 
    txt.extend(pre_txt)

    # (*) Access input character etc. _________________________________________
    specific.framework(txt, TheState, TheAnalyzer)

    # (*) Transition Map ______________________________________________________
    tm = MegaState_relate_to_transition_code(TheState, TheAnalyzer, specific.state_key_str)
    transition_block.do(txt, tm)

    # (*) Stuff to be pasted after transition map
    txt.extend(post_txt)

    # (*) Request necessary variable definition _______________________________
    specific.require_data(TheState, TheAnalyzer)

    # Undo defines from [X]
    specific.debug_info_undo_map_state_key_to_state_index(txt)
    return


