
def do(A, B):
    """Computes 'gain' with respect to entry actions, if two states are
    combined.
    """
    # Every different command list requires a separate door.
    # => Entry cost is proportional to number of unique command lists.
    # => Gain =   number of unique command lists of A an B each
    #           - number of unique command lists of Combined(A, B)
    A_unique_cl_set = set(ta.command_list for ta in A.action_db.itervalues())
    B_unique_cl_set = set(ta.command_list for ta in B.action_db.itervalues())
    # (1) Compute sizes BEFORE setting Combined_cl_set = A_unique_cl_set
    A_size = len(A_unique_cl_set)
    B_size = len(B_unique_cl_set)
    # (2) Compute combined cost
    Combined_cl_set = A_unique_cl_set  # reuse 'A_unique_cl_set'
    Combined_cl_set.update(B_unique_cl_set)
    return A_size + B_size - len(Combined_cl_set)
    
