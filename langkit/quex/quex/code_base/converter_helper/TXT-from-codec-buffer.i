/* -*- C++ -*- vim: set syntax=cpp:
 * PURPOSE: 
 *
 * Provide the implementation of character and string converter functions
 * FROM the buffer's $$CODEC$$ to utf8, utf16, utf32, char, and wchar_t.
 *
 * STEPS:
 *
 * (1) Implement the character converters from buffer's $$CODEC$$ to 
 *     utf8, utf16, utf32. Those come out of quex's code generator.
 *
 * (1b) Derive the converts from $$CODEC$$ to char and wchar_t from
 *      those converters. For this use:
 *
 *          "../generator/character-converter-char-wchar_t.gi"
 *
 * (2) Generate the implementation of the string converters in terms
 *     of those character converters.
 *
 *     Use: "../generator/implementation-string-converters.gi"
 *
 *          which uses
 *
 *              "../generator/string-converter.gi"
 *
 *          to implement each string converter from the given 
 *          character converters. 
 *
 * These functions ARE DEPENDENT on QUEX_TYPE_CHARACTER.
 * => Thus, they are placed in the analyzer's namespace.
 *
 * 2010 (C) Frank-Rene Schaefer; 
 * ABSOLUTELY NO WARRANTY                                                    */
#if    ! defined(__QUEX_INCLUDE_GUARD__CONVERTER_HELPER__$$CODEC$$_I) \
    ||   defined(__QUEX_INCLUDE_GUARD__CONVERTER_HELPER__TMP_DISABLED)
#if    ! defined(__QUEX_INCLUDE_GUARD__CONVERTER_HELPER__TMP_DISABLED)
#        define  __QUEX_INCLUDE_GUARD__CONVERTER_HELPER__$$CODEC$$_I
#endif

#include "$$CODEC_HEADER$$"

QUEX_NAMESPACE_MAIN_OPEN

QUEX_INLINE void
QUEX_CONVERTER_CHAR_DEF($$CODEC$$, utf32)(const QUEX_TYPE_CHARACTER** input_pp,
                                          uint32_t**                  output_pp)
{
    uint16_t             unicode = (uint32_t)0;
    QUEX_TYPE_CHARACTER  input   = *(*input_pp)++;
$$BODY_UTF32$$
}

QUEX_INLINE void
QUEX_CONVERTER_CHAR_DEF($$CODEC$$, utf16)(const QUEX_TYPE_CHARACTER** input_pp,
                                          uint16_t**                  output_pp)
{
    uint32_t   unicode   = (uint32_t)0;
    uint32_t*  unicode_p = &unicode;

    QUEX_CONVERTER_CHAR($$CODEC$$, utf32)(input_pp, &unicode_p);
$$BODY_UTF16$$
}

QUEX_INLINE void
QUEX_CONVERTER_CHAR_DEF($$CODEC$$, utf8)(const QUEX_TYPE_CHARACTER**  input_pp, 
                                         uint8_t**                    output_pp)
{
    uint32_t            unicode = (uint32_t)-1;
    QUEX_TYPE_CHARACTER input   = *(*input_pp)++;
    
$$BODY_UTF8$$

$$EPILOG$$
}

#define __QUEX_FROM           $$CODEC$$
#define __QUEX_FROM_TYPE      QUEX_TYPE_CHARACTER

/* (1b) Derive converters to char and wchar_t from the given set 
 *      of converters. (Generator uses __QUEX_FROM and QUEX_FROM_TYPE)      */
#include <quex/code_base/converter_helper/generator/character-converter-to-char-wchar_t.gi>

/* (2) Generate string converters to utf8, utf16, utf32 based on the
 *     definitions of the character converters.                             */
#include <quex/code_base/converter_helper/generator/implementations.gi>

QUEX_NAMESPACE_MAIN_CLOSE

#endif /* __QUEX_INCLUDE_GUARD__CONVERTER_HELPER__$$CODEC$$_I */

