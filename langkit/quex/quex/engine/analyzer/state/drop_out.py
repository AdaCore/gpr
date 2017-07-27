from   quex.engine.operations.operation_list import OpList, \
                                        Op

def get_OpList(TheAccepter, TheTerminalRouter):
    """If there is no stored acceptance involved, then one can directly
    conclude from the pre-contexts to the acceptance_id. Then the drop-
    out action can be described as a sequence of checks

       # [0] Check          [1] Position and Goto Terminal
       if   pre_context_32: input_p = x; goto terminal_893;
       elif pre_context_32: goto terminal_893;
       elif pre_context_32: input_p = x; goto terminal_893;
       elif pre_context_32: goto terminal_893;

    Such a configuration is considered trivial. No restore is involved.

    RETURNS: None                                          -- if not trivial
             list((pre_context_id, TerminalRouterElement)) -- if trivial
    """
    # If the 'last_acceptance' is not determined in this state, then it
    # must bee derived from previous storages. We cannot simplify here.
    if TheAccepter is None: 
        return OpList(TheTerminalRouter)

    elif not TheAccepter.content.has_acceptance_without_pre_context():
        # If no pre-context is met, then 'last_acceptance' needs to be 
        # considered.
        return OpList(TheAccepter, TheTerminalRouter)

    def router_element(TerminalRouter, AcceptanceId):
        for x in TerminalRouter:
            if x.acceptance_id == AcceptanceId: return x
        assert False  # There MUST be an element for each acceptance_id!

    router = TheTerminalRouter.content

    return OpList.from_iterable(
        Op.IfPreContextSetPositionAndGoto(check.pre_context_id, 
                                          router_element(router, check.acceptance_id))
        for check in TheAccepter.content
    )


