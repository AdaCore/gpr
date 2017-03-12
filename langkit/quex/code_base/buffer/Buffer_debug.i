/* -*- C++ -*-  vim: set syntax=cpp:
 *
 * (C) 2008 Frank-Rene Schaefer */
#ifndef __QUEX_INCLUDE_GUARD__BUFFER__BUFFER_DEBUG_I
#define __QUEX_INCLUDE_GUARD__BUFFER__BUFFER_DEBUG_I

#include <quex/code_base/definitions>
#include <quex/code_base/buffer/Buffer>
#include <quex/code_base/buffer/BufferFiller>
#include <quex/code_base/buffer/asserts>
#include <quex/code_base/buffer/Buffer_debug>


QUEX_NAMESPACE_MAIN_OPEN

    QUEX_INLINE void 
    QUEX_NAME(Buffer_show_brief_content)(QUEX_NAME(Buffer)* buffer) 
    {
        QUEX_NAME(BufferFiller)* me = buffer->filler;

        __quex_assert(buffer != 0x0);
        __quex_assert(me != 0x0);

        QUEX_BUFFER_ASSERT_CONSISTENCY(buffer);
        __QUEX_STD_printf("Begin of Buffer Character Index: %i\n", (int)buffer->_content_character_index_begin);
        __QUEX_STD_printf("End   of Buffer Character Index: %i\n", (int)me->tell_character_index(me));
        if( buffer->_memory._end_of_file_p == 0x0 )
            __QUEX_STD_printf("_memory._memory._end_of_file_p (offset)  = <0x0>\n");
        else
            __QUEX_STD_printf("_memory._end_of_file_p (offset)  = %08X\n", 
                              (int)(buffer->_memory._end_of_file_p  - buffer->_memory._front));
        __QUEX_STD_printf("_input_p (offset)        = %08X\n", (int)(buffer->_input_p        - buffer->_memory._front));
        __QUEX_STD_printf("_lexeme_start_p (offset) = %08X\n", (int)(buffer->_lexeme_start_p - buffer->_memory._front));
        __QUEX_STD_printf("_back (offset)           = %08X\n", (int)(buffer->_memory._back   - buffer->_memory._front));
    }

    QUEX_INLINE void 
    QUEX_NAME(Buffer_x_show_content)(QUEX_NAME(Buffer)* buffer) 
    {
        QUEX_NAME(Buffer_show_content_intern)(buffer);
        QUEX_NAME(Buffer_show_brief_content)(buffer);
    }

    QUEX_INLINE QUEX_TYPE_CHARACTER
    QUEX_NAME(__Buffer_get_border_char)(QUEX_NAME(Buffer)* buffer, const QUEX_TYPE_CHARACTER* C) 
    {
        if     ( *C != QUEX_SETTING_BUFFER_LIMIT_CODE )   
            return (QUEX_TYPE_CHARACTER)'?'; 
        else if( buffer->_memory._end_of_file_p == C )       
            return (QUEX_TYPE_CHARACTER)']';
        else if( buffer->_content_character_index_begin == 0 && buffer->_memory._front == C )     
            return (QUEX_TYPE_CHARACTER)'[';
        else
            return (QUEX_TYPE_CHARACTER)'|';
    }

    QUEX_INLINE void
    QUEX_NAME(Buffer_show_content_intern)(QUEX_NAME(Buffer)* buffer)
    {
        size_t                i = 0;
        size_t                length = 0;

        QUEX_TYPE_CHARACTER   EmptyChar    = (QUEX_TYPE_CHARACTER)(-1);
        QUEX_TYPE_CHARACTER*  ContentFront = QUEX_NAME(Buffer_content_front)(buffer);
        QUEX_TYPE_CHARACTER*  BufferFront  = buffer->_memory._front;
        QUEX_TYPE_CHARACTER*  BufferBack   = buffer->_memory._back;
        QUEX_TYPE_CHARACTER*  iterator = 0x0;
        QUEX_TYPE_CHARACTER*  end_p    = buffer->_memory._end_of_file_p != 0x0 ? buffer->_memory._end_of_file_p 
                                         :                                       buffer->_memory._back;

        __QUEX_STD_printf("|%c", (int)QUEX_NAME(__Buffer_get_border_char)(buffer, BufferFront));
        for(iterator = ContentFront; iterator != end_p; ++iterator) {
            __QUEX_STD_printf("%c", *iterator == EmptyChar ? (int)'~' : (int)*iterator);
        }
        __QUEX_STD_printf("%c", (int)QUEX_NAME(__Buffer_get_border_char)(buffer, end_p));
        /**/
        length = (buffer->_memory._end_of_file_p == 0x0) ? 0 : (size_t)(BufferBack - buffer->_memory._end_of_file_p);
        for(i=0; i < length; ++i) __QUEX_STD_printf("|");

        __QUEX_STD_printf("|");
    }

    QUEX_INLINE void  
    QUEX_NAME(Buffer_show_content)(QUEX_NAME(Buffer)* buffer) 
    {
        /* NOTE: If the limiting char needs to be replaced temporarily by
         *       a terminating zero.
         * NOTE: This is a **simple** printing function for unit testing and debugging
         *       it is thought to print only ASCII characters (i.e. code points < 0xFF)*/
        size_t                i = 0;
        char*                 tmp = 0;
        const size_t          ContentSize  = QUEX_NAME(Buffer_content_size)(buffer);
        QUEX_TYPE_CHARACTER*  ContentFront = QUEX_NAME(Buffer_content_front)(buffer);
        QUEX_TYPE_CHARACTER*  BufferFront  = buffer->_memory._front;
        QUEX_TYPE_CHARACTER*  BufferBack   = buffer->_memory._back;

        __quex_assert(buffer != 0x0);
        /*_________________________________________________________________________________*/
        tmp = (char*)__QUEX_STD_malloc(ContentSize + 4);
        /* tmp[0]                 = outer border*/
        /* tmp[1]                 = buffer limit*/
        /* tmp[2...ContentSize+1] = ContentFront[0...ContentSize-1]*/
        /* tmp[ContentSize+2]     = buffer limit*/
        /* tmp[ContentSize+3]     = outer border*/
        /* tmp[ContentSize+4]     = terminating zero*/
        for(i=2; i<ContentSize + 2 ; ++i) tmp[i] = ' ';
        tmp[ContentSize+4] = '\0';
        tmp[ContentSize+3] = '|';
        tmp[ContentSize+2] = (char)QUEX_NAME(__Buffer_get_border_char)(buffer, BufferBack);
        tmp[1]             = (char)QUEX_NAME(__Buffer_get_border_char)(buffer, BufferFront);
        tmp[0]             = '|';
        /* tmp[_SHOW_current_fallback_n - 1 + 2] = ':';        */
        tmp[buffer->_input_p - ContentFront + 2] = 'C';
        if( buffer->_lexeme_start_p >= ContentFront && buffer->_lexeme_start_p <= BufferBack ) 
            tmp[(int)(buffer->_lexeme_start_p - ContentFront) + 2] = 'S';
        /**/
        if ( buffer->_input_p == ContentFront - 2 ) {
            __QUEX_STD_printf("%s", tmp); 
            __QUEX_STD_printf(" <out>");
        } else {
            __QUEX_STD_printf(" ");
            if( *buffer->_input_p == QUEX_SETTING_BUFFER_LIMIT_CODE ) 
                __QUEX_STD_printf("BLC");
            else                                  
                __QUEX_STD_printf("'%c'", (char)(*buffer->_input_p));
        }
        /* std::cout << " = 0x" << std::hex << int(*buffer->_input_p) << std::dec */
        __QUEX_STD_printf("\n");
        QUEX_NAME(Buffer_show_content_intern)(buffer);
        __QUEX_STD_free(tmp);
    }

    QUEX_INLINE void  
    QUEX_NAME(Buffer_show_byte_content)(QUEX_NAME(Buffer)* buffer, const int IndentationN) 
    {
        QUEX_NAME(BufferMemory)*  memory = &buffer->_memory;

        int      i = 0, j = 0;
        uint8_t* byte_p      = (uint8_t*)memory->_front;
        uint8_t* next_byte_p = (uint8_t*)memory->_front + 1;
        uint8_t* End         = (uint8_t*)(memory->_back + 1);

        __quex_assert(buffer != 0x0);
        __quex_assert(memory != 0x0);

        for(j=0; j<IndentationN; ++j) fprintf(stdout, " ");
        for(; byte_p != End; ++byte_p, ++next_byte_p, ++i) {
            fprintf(stdout, "%02X", (int)*byte_p);
            if     ( next_byte_p == (uint8_t*)buffer->_memory._end_of_file_p ) 
                fprintf(stdout, "[");
            else if( byte_p      == (uint8_t*)buffer->_memory._end_of_file_p + sizeof(QUEX_TYPE_CHARACTER)-1) 
                fprintf(stdout, "]");
            else 
                fprintf(stdout, ".");
            if( (i+1) % 0x8  == 0 ) fprintf(stdout, " ");
            if( (i+1) % 0x10 == 0 ) { 
                fprintf(stdout, "\n");
                for(j=0; j<IndentationN; ++j) fprintf(stdout, " ");
            }
        }
        fprintf(stdout, "\n");
    }

    QUEX_INLINE void  
    QUEX_NAME(Buffer_show_debug_print_lines)(QUEX_TYPE_CHARACTER** iterator, 
                                             QUEX_TYPE_CHARACTER*  Begin, 
                                             QUEX_TYPE_CHARACTER*  TotalEnd, 
                                             QUEX_NAME(Buffer)*    buffer)
    {
        int                   length = 0;
        QUEX_TYPE_CHARACTER*  end = Begin + 5 > TotalEnd ? TotalEnd : Begin + 5;

        if( Begin > *iterator ) {
            *iterator = Begin;
            __QUEX_STD_fprintf(stderr, "                                           ...\n");
        } else if( *iterator >= end ) {
            return;
        }

        for(; *iterator < end; ++*iterator) {
            length = 0;
            __QUEX_STD_fprintf(stderr, "   ");

            if( *iterator == buffer->_memory._front ) {
                __QUEX_STD_fprintf(stderr, "buffer front");
                length += 12;
            }
            if( *iterator == buffer->_lexeme_start_p ) {
                if( length ) { __QUEX_STD_fprintf(stderr, ", "); length += 2; }
                __QUEX_STD_fprintf(stderr, "lexeme start");
                length += 12;
            }
            if( *iterator == buffer->_input_p ) {
                if( length ) { __QUEX_STD_fprintf(stderr, ", "); length += 2; }
                __QUEX_STD_fprintf(stderr, "input");
                length += 5;
            }
            if( *iterator == buffer->_memory._end_of_file_p ) {
                if( length ) { __QUEX_STD_fprintf(stderr, ", "); length += 2; }
                __QUEX_STD_fprintf(stderr, "end of file");
                length += 11;
            }
            if( *iterator == buffer->_memory._back ) {
                if( length ) { __QUEX_STD_fprintf(stderr, ", "); length += 2; }
                __QUEX_STD_fprintf(stderr, "buffer back");
                length += 11;
            }
            if( length ) {
                for(; length < 39; ++length)
                    __QUEX_STD_fprintf(stderr, "-");
                __QUEX_STD_fprintf(stderr, ">");
            } else {
                __QUEX_STD_fprintf(stderr, "                                        ");
            }

            /* Print the character information */
            __QUEX_STD_fprintf(stderr, "[%04X] 0x%04X\n", 
                               (int)(*iterator - buffer->_memory._front),
                               (int)(**iterator));
        }
    }

    QUEX_INLINE void  
    QUEX_NAME(Buffer_show_debug_content)(QUEX_NAME(Buffer)* buffer) 
    {
        /* Assumptions: 
         *    (1) width of terminal     = 80 chars
         *    (2) border right and left = 3 chars
         *    (3) display at least the last 5 chars at the begin of buffer.
         *                                  5 chars around input_p.
         *                                  5 chars from lexeme_start.
         *                                  5 chars to the end of buffer.
         *
         *    |12345 ...      12345  ....       12345      ....    12345|
         *    Begin           lexeme start        input_p               buffer end     */ 
        QUEX_TYPE_CHARACTER*  iterator  = buffer->_memory._front;
        QUEX_TYPE_CHARACTER*  total_end = buffer->_memory._back + 1;

        __quex_assert(buffer != 0x0);
        __QUEX_STD_fprintf(stderr, "_________________________________________________________________\n");
        QUEX_NAME(Buffer_show_debug_print_lines)(&iterator, buffer->_memory._front,      total_end, buffer);
        QUEX_NAME(Buffer_show_debug_print_lines)(&iterator, buffer->_lexeme_start_p - 2, total_end, buffer);
        QUEX_NAME(Buffer_show_debug_print_lines)(&iterator, buffer->_input_p        - 2, total_end, buffer);
        if( buffer->_memory._end_of_file_p != 0x0 ) {
            QUEX_NAME(Buffer_show_debug_print_lines)(&iterator, buffer->_memory._end_of_file_p - 4, total_end, buffer);
        }
        QUEX_NAME(Buffer_show_debug_print_lines)(&iterator, buffer->_memory._back   - 4, total_end, buffer);
        __QUEX_STD_fprintf(stderr, "_________________________________________________________________\n");
    }

QUEX_NAMESPACE_MAIN_CLOSE


#endif /* __QUEX_INCLUDE_GUARD__BUFFER__BUFFER_DEBUG_I */
