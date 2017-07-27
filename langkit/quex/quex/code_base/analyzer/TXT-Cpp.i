#include <quex/code_base/temporary_macros_on>

QUEX_NAMESPACE_MAIN_OPEN

#ifndef __QUEX_OPTION_PLAIN_C
TEMPLATE_IN(InputHandleT)
#endif
void
QUEX_NAME(constructor_core)(QUEX_TYPE_ANALYZER*    me,
                            InputHandleT*          input_handle, 
                            const char*            CharacterEncodingName,
                            bool                   ByteOrderReversionF,
                            QUEX_TYPE_CHARACTER*   BufferMemory,    
                            size_t                 BufferMemorySize,
                            QUEX_TYPE_CHARACTER*   BufferEndOfFileP)
{
$$CONSTRUCTOR_MODE_DB_INITIALIZATION_CODE$$

    QUEX_NAME(construct_basic)(me, input_handle,
                               BufferMemory, BufferMemorySize, BufferEndOfFileP,
                               CharacterEncodingName, 
                               QUEX_SETTING_TRANSLATION_BUFFER_SIZE,
                               ByteOrderReversionF);

    me->__current_mode_p = 0x0; /* REQUIRED, for mode transition check */
    QUEX_NAME(set_mode_brutally_by_id)(me, __QUEX_SETTING_INITIAL_LEXER_MODE_ID);

#define self  (*(QUEX_TYPE_DERIVED_ANALYZER*)me)
/* START: User's constructor extensions _______________________________________*/
$$CONSTRUCTOR_EXTENSTION$$
/* END: _______________________________________________________________________*/
#undef self
}


#ifdef QUEX_OPTION_INCLUDE_STACK

#ifndef __QUEX_OPTION_PLAIN_C
TEMPLATE_IN(InputHandleT)
#endif
QUEX_NAME(Memento)*
QUEX_NAME(memento_pack)(QUEX_TYPE_ANALYZER*   me, 
                        QUEX_TYPE_CHARACTER*  InputName, 
                        InputHandleT**        input_handle)
{
#   define self  (*me)
    QUEX_NAME(Memento)* memento = (QUEX_NAME(Memento)*)QUEXED(MemoryManager_allocate)(
                                     sizeof(QUEX_NAME(Memento)), QUEXED(MemoryObjectType_MEMENTO));
    
    (void)InputName;
    (void)input_handle;

#   ifndef __QUEX_OPTION_PLAIN_C
    /* Use placement 'new' for explicit call of constructor. 
     * Necessary in C++: Trigger call to constructor for user defined members.   */
    new ((void*)memento) QUEX_NAME(Memento);
#   endif

    memento->_parent_memento                  = self._parent_memento;
    memento->buffer                           = self.buffer;
    memento->__current_mode_p                 = self.__current_mode_p; 
    memento->current_analyzer_function        = self.current_analyzer_function;
#   if    defined(QUEX_OPTION_AUTOMATIC_ANALYSIS_CONTINUATION_ON_MODE_CHANGE) \
       || defined(QUEX_OPTION_ASSERTS)
    memento->DEBUG_analyzer_function_at_entry = self.DEBUG_analyzer_function_at_entry;
#   endif
    __QUEX_IF_COUNT( memento->counter         = self.counter);
#   ifdef QUEX_OPTION_STRING_ACCUMULATOR
    memento->accumulator                      = self.accumulator;
#   endif
    memento->__file_handle_allocated_by_constructor = self.__file_handle_allocated_by_constructor;

    /* Deriberately not subject to include handling:
     *    -- Mode stack.
     *    -- Token and token queues.
     *    -- Post categorizer.                                                 */

#   ifdef QUEX_OPTION_TOKEN_POLICY_QUEUE
    /* QuexTokenQueueRemainder_restore(&memento->token_queue_remainder, &self._token_queue); */
#   endif

/* START: User's memento 'pack' _______________________________________________*/
$$MEMENTO_EXTENSIONS_PACK$$
/* END: _______________________________________________________________________*/

    return memento;
#   undef self
}

#ifndef __QUEX_OPTION_PLAIN_C
QUEX_INLINE 
#endif
void
QUEX_NAME(memento_unpack)(QUEX_TYPE_ANALYZER*  me, 
                          QUEX_NAME(Memento)*  memento)
{
#   define self  (*me)
    self._parent_memento                  = memento->_parent_memento;
    self.buffer                           = memento->buffer;
    self.__current_mode_p                 = memento->__current_mode_p; 
    self.current_analyzer_function        = memento->current_analyzer_function;
#   if    defined(QUEX_OPTION_AUTOMATIC_ANALYSIS_CONTINUATION_ON_MODE_CHANGE) \
       || defined(QUEX_OPTION_ASSERTS)
    self.DEBUG_analyzer_function_at_entry = memento->DEBUG_analyzer_function_at_entry;
#   endif
    __QUEX_IF_COUNT(self.counter          = memento->counter);
#   ifdef QUEX_OPTION_STRING_ACCUMULATOR
    self.accumulator                      = memento->accumulator;
#   endif
    self.__file_handle_allocated_by_constructor = memento->__file_handle_allocated_by_constructor;

#   ifdef QUEX_OPTION_TOKEN_POLICY_QUEUE
    /* QuexTokenQueueRemainder_restore(&memento->token_queue_remainder, &self._token_queue); */
#   endif

/* START: User's memento 'unpack' _____________________________________________*/
$$MEMENTO_EXTENSIONS_UNPACK$$
/* END: _______________________________________________________________________*/
    
#   ifndef __QUEX_OPTION_PLAIN_C
    /* Counterpart to placement new: Explicit destructor call.
     * Necessary in C++: Trigger call to destructor for user defined members.  */
    memento->~QUEX_NAME(Memento_tag)();
#   endif

    QUEXED(MemoryManager_free)((void*)memento, QUEXED(MemoryObjectType_MEMENTO)); 
#   undef self
}
#endif /* QUEX_OPTION_INCLUDE_STACK */

QUEX_NAMESPACE_MAIN_CLOSE

#include <quex/code_base/temporary_macros_off>

#if defined(__QUEX_OPTION_CONVERTER_HELPER)
#   include "$$CONVERTER_HELPER_I$$"
#else
#   include "quex/code_base/converter_helper/from-unicode-buffer.i"
#endif
#include <quex/code_base/analyzer/headers.i>

