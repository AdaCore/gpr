from itertools import ifilter

_deactivated_for_unit_test_f = False

def do(sm):
    """Cuts any path element after acceptance states. This is appropriate 
       for pre-context detectors, post-contexts (that are not pseudo-ambigous),
       and backward input position detectors.
    """
    global _deactivated_for_unit_test_f

    if _deactivated_for_unit_test_f: return

    for state in ifilter(lambda x: x.is_acceptance(), sm.states.itervalues()):
        state.target_map.clear()

    orphan_list = sm.get_orphaned_state_index_list()
    while len(orphan_list) != 0:
        for state_index in orphan_list:
            del sm.states[state_index]
        orphan_list = sm.get_orphaned_state_index_list()


