#ifndef __QUEX_INCLUDE_GUARD__ANALYZER__DUMPSTER_I
#define __QUEX_INCLUDE_GUARD__ANALYZER__DUMPSTER_I
/*
    // (*) utilities for string triming
    //
    //     NOTE: Any better linker should be able to inline the 
    //           condition functions, so do not worry about the 
    //           function pointers.
    inline void
    trim_rear(QUEX_LEXEME_CHARACTER_TYPE*     text, 
              const int Length,
              int       (*condition)(const QUEX_TYPE_CHARACTER))
        // PURPOSE:
        //    Inserts a terminating zero after the first letter where a
        //    condition hold. This is useful, for example, if strings have 
        //    to be created and whitespace at borders shall not take space.
        //
        //    The length of the text may already been determined. Pass Length,
        //    so no call to strlen is necessary here.
        //
        // text       = text to be parsed for condition
        // Length     = Length of the string
        // condition  = pointer to function that checks for condition
        //              return 1 = condition holds
        //              return 0 = condition does not hold
        //              (examples: the functions isspace(..), islower(..)
        //                         from standard <ctype.h> header)
    {
        QUEX_LEXEME_CHARACTER_TYPE* p = text + Length - 1;
        for(; p > text && (*condition)(*p); --p);
        *(++p) = '\0'; // terminating zero
    }

    inline const QUEX_LEXEME_CHARACTER_TYPE*
    trim_front(QUEX_LEXEME_CHARACTER_TYPE* text, 
               int   (*condition)(const QUEX_TYPE_CHARACTER))
        // PURPOSE:
        //    Like insert_term_zero_after_condition(...) treating the string
        //    from front to back (i.e. vice versa). Does not insert terminating
        //    zero, but returns the pointer to the first non-whitespace.
        //
        // RETURNS:
        //    pointer to first character that is not whitespace.
        //
    {       
        QUEX_LEXEME_CHARACTER_TYPE* p = text; 
        for(; *p && (*condition)(*p); ++p);
        return p;
    }

    inline const QUEX_LEXEME_CHARACTER_TYPE*
    trim(QUEX_LEXEME_CHARACTER_TYPE*     text, 
         const int Length,
         int       (*condition)(const QUEX_TYPE_CHARACTER))
        // PURPOSE:
        //    Delivers a string that has no whitespace at front and none at end.
        //    For example, the string "  hello world!    " becomes "hello world!".
        //
        //    The length of the text may already been determined, so no call
        //    to strlen is necessary here.
        //
        // ARGUMENTS:
        //    text      = text to be trimmed. 
        //    Length    = length of the original string.
        //    condition = condition function
        //
        // ATTENTION: The same memory of text is used for the return value. 
        //            But, it is modified! The content of 'text' may not
        //            be the same after this function call, since a terminating
        //            zero is forced after the last non-whitespace character.
        //
        // RETURNS:
        //    Pointer to the first non-whitespace character of text. At the
        //    end of the last non-whitespace character, there is going to be
        //    a '\0' determining the string border.
        //
    { trim_rear(text, Length, condition); return trim_front(text, condition); }

    inline void
    trim_rear(QUEX_LEXEME_CHARACTER_TYPE* text, const int Length)
    { trim_rear(text, Length, &isspace); }

    inline const QUEX_LEXEME_CHARACTER_TYPE*
    trim_front(QUEX_LEXEME_CHARACTER_TYPE* text)
    { trim_front(text, &isspace); }

    inline const QUEX_LEXEME_CHARACTER_TYPE*
    trim(QUEX_LEXEME_CHARACTER_TYPE* text, const int Length)
    { return trim(text, Length, &isspace); }
*/


typedef struct QUEX_NAME(BufferFillerExtern_tag) {

    QUEX_NAME(BufferFiller)* filler;

    QUEX_TYPE_CHARACTER*  insert(QUEX_NAME(BufferFillerExtern_tag)*  me,
                                 QUEX_TYPE_ANALYZER*                 the_lexer,
                                 QUEX_TYPE_CHARACTER*                insertion_p,
                                 QUEX_TYPE_EXT_CHARACTER*            ContentBegin,
                                 QUEX_TYPE_EXT_CHARACTER*            ContentEnd);

    QUEX_TYPE_CHARACTER*  append(QUEX_NAME(BufferFillerExtern_tag)*  me,
                                 QUEX_TYPE_ANALYZER*                 the_lexer,
                                 QUEX_TYPE_CHARACTER*                ContentBegin, 
                                 QUEX_TYPE_CHARACTER*                ContentEnd);

    uint8_t*              append_conversion(QUEX_NAME(BufferFillerExtern_tag)*  me,
                                            QUEX_TYPE_ANALYZER*                 the_lexer,
                                            uint8_t*                            ContentBegin, 
                                            uint8_t*                            ContentEnd);

    uint8_t*              append_conversion_direct(QUEX_NAME(BufferFillerExtern_tag)*  me,
                                                   QUEX_TYPE_ANALYZER*                 the_lexer,
                                                   uint8_t*                            ContentBegin, 
                                                   uint8_t*                            ContentEnd);

    void                  prepare(QUEX_NAME(BufferFillerExtern_tag)*  me,
                                  QUEX_TYPE_ANALYZER*                 the_lexer);

    void                  finish(QUEX_NAME(BufferFillerExtern_tag)*  me,
                                 QUEX_TYPE_ANALYZER*                 the_lexer,
                                 const size_t                        LoadedN);

    QUEX_TYPE_CHARACTER*  fill_region_begin(QUEX_TYPE_ANALYZER*               the_lexer);
    QUEX_TYPE_CHARACTER*  fill_region_end(QUEX_NAME(BufferFillerExtern_tag)*  me,
                                          QUEX_TYPE_ANALYZER*                 the_lexer);
    size_t                fill_region_size(QUEX_NAME(BufferFillerExtern_tag)* me,
                                           QUEX_TYPE_ANALYZER*                the_lexer);

    void*                 delete_self(QUEX_NAME(BufferFillerExtern_tag)*);
    
} QUEX_NAME(BufferFillerExtern);
#endif /* __QUEX_INCLUDE_GUARD__ANALYZER__DUMPSTER_I */
