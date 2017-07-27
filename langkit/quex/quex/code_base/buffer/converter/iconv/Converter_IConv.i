/* -*- C++ -*-  vim: set syntax=cpp:
 * (C) 2007-2008 Frank-Rene Schaefer  */
#ifndef __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__ICONV__CONVERTER_ICONV_I
#define __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__ICONV__CONVERTER_ICONV_I

#ifndef __QUEX_OPTION_PLAIN_C
extern "C" { 
#endif
#include <errno.h>
#ifndef __QUEX_OPTION_PLAIN_C
}
#endif
#include <quex/code_base/definitions>
#include <quex/code_base/compatibility/iconv-argument-types.h>
#include <quex/code_base/MemoryManager>
#include <quex/code_base/buffer/converter/iconv/Converter_IConv>

#if ! defined(QUEX_OPTION_CONVERTER_ICONV)
#    error "This header has been included without setting the compile option QUEX_OPTION_CONVERTER_ICONV. This could cause problems on systems where the correspondent headers are not installed. Make the inclusion of this header dependent on the above compile option."
#endif

#include <quex/code_base/analyzer/configuration/validation>

QUEX_NAMESPACE_MAIN_OPEN

    QUEX_INLINE void 
    QUEX_NAME(Converter_IConv_open)(QUEX_NAME(Converter_IConv)* me,
                                    const char* FromCoding, const char* ToCoding)
    {
        /* Default: assume input encoding to have dynamic character sizes. */
        me->base.dynamic_character_size_f = true;

        /* Setup conversion handle */
        if( ToCoding == 0 ) {
            switch( sizeof(QUEX_TYPE_CHARACTER) ) {
            default:  __quex_assert(false); return;
#           if   defined (__QUEX_OPTION_SYSTEM_ENDIAN) 
            case 4:  me->handle = iconv_open("UCS-4LE",  FromCoding);  break;
            case 2:  me->handle = iconv_open("UCS-2LE",  FromCoding);  break;
#           elif defined(__QUEX_OPTION_LITTLE_ENDIAN)
            case 4:  me->handle = iconv_open("UCS-4LE",  FromCoding);  break;
            case 2:  me->handle = iconv_open("UCS-2LE",  FromCoding);  break;
#           elif defined(__QUEX_OPTION_BIG_ENDIAN)
            case 4:  me->handle = iconv_open("UCS-4BE",  FromCoding);  break;
            case 2:  me->handle = iconv_open("UCS-2BE",  FromCoding);  break;
#           endif
            case 1:  me->handle = iconv_open("ASCII",    FromCoding);  break;
            }
        } else {
            me->handle = iconv_open(ToCoding, FromCoding);
        }

        if( me->handle == (iconv_t)-1 ) {
            /* __QUEX_STD_fprintf(stderr, "Source coding: '%s'\n", FromCoding);
             * __QUEX_STD_fprintf(stderr, "Target coding: '%s'\n", ToCoding);  */
            QUEX_ERROR_EXIT("<<IConv conversion: source or target character encoding name unknown.>>");
        }
    }

    QUEX_INLINE bool 
    QUEX_NAME(Converter_IConv_convert)(QUEX_NAME(Converter_IConv)*   me, 
                                       uint8_t**              source, const uint8_t*              SourceEnd,
                                       QUEX_TYPE_CHARACTER**  drain,  const QUEX_TYPE_CHARACTER*  DrainEnd)
    {
        /* RETURNS:  true  --> User buffer is filled as much as possible with converted 
         *                     characters.
         *           false --> More raw bytes are needed to fill the user buffer.           
         *
         *  IF YOU GET A COMPILE ERROR HERE, THEN PLEASE HAVE A LOOK AT THE FILE:
         *
         *      quex/code_base/compatibility/iconv-argument-types.h
         * 
         *  The issue is, that 'iconv' is defined on different systems with different
         *  types of the second argument. There are two variants 'const char**'
         *  and 'char **'.  If you get an error here, consider defining 
         *
         *            -DQUEX_SETTING_ICONV_2ND_ARG_CONST_CHARPP
         *
         *  as a compile option. If you have an elegant solution to solve the problem for 
         *  plain 'C', then please, let me know <fschaef@users.sourceforge.net>.               */
        size_t source_bytes_left_n = (size_t)(SourceEnd - *source);
        size_t drain_bytes_left_n  = (size_t)(DrainEnd - *drain)*sizeof(QUEX_TYPE_CHARACTER);

        size_t report = iconv(me->handle, 
                              __QUEX_ADAPTER_ICONV_2ND_ARG(source), &source_bytes_left_n,
                              (char**)drain,                        &drain_bytes_left_n);

        if( report != (size_t)-1 ) { 
            __quex_assert(source_bytes_left_n == 0);
            /* The input sequence (raw buffer content) has been converted completely.
             * But, is the user buffer filled to its limits?                                   */
            if( drain_bytes_left_n == 0 ) {
                __quex_assert(*drain == DrainEnd);
                return true; 
            }
            /* If the buffer was not filled completely, then was it because we reached EOF?
             * NOTE: Here, 'source->iterator' points to the position after the last byte
             *       that has been converted. If this is the end of the buffer, then it means
             *       that the raw buffer was read. If not, it means that the buffer has not been
             *       filled to its border which happens only if End of File occured.           */
            if( *source != SourceEnd ) {
                /*__quex_assert(me->raw_buffer.end != me->raw_buffer.memory_end);*/
                return true;
            }
            else {
                /* Else: The user buffer is still hungry, thus the raw buffer needs more bytes. */
                /* *source == SourceEnd anyway, so 'refill' is triggered at any time.           */
                return false; 
            }
        }

        switch( errno ) {
        default:
            QUEX_ERROR_EXIT("Unexpected setting of 'errno' after call to GNU's iconv().");

        case EILSEQ:
            QUEX_ERROR_EXIT("Invalid byte sequence encountered for given character coding.");

        case EINVAL:
            /* Incomplete byte sequence for character conversion
             * ('raw_buffer.iterator' points to the beginning of the incomplete sequence.)
             * Please, refill the buffer (consider copying the bytes from raw_buffer.iterator 
             * to the end of the buffer in front of the new buffer).                               
             * If it happens, that we just finished filling the drain buffer before this happend
             * than the 'read_characters()' function does not need to reload.                    */
            if( *drain == DrainEnd ) return true;
            else                     return false; 

        case E2BIG:
            /* The input buffer was not able to hold the number of converted characters.
             * (in other words we're filled up to the limit and that's what we actually wanted.) */
            return true;
        }
    }

    QUEX_INLINE void 
    QUEX_NAME(Converter_IConv_delete_self)(QUEX_NAME(Converter_IConv)* me)
    {
        iconv_close(me->handle); 
        QUEXED(MemoryManager_free)((void*)me, QUEXED(MemoryObjectType_CONVERTER));
    }

    QUEX_INLINE QUEX_NAME(Converter)*
    QUEX_NAME(Converter_IConv_new)()
    {
        QUEX_NAME(Converter_IConv)*  me = \
           (QUEX_NAME(Converter_IConv)*)
           QUEXED(MemoryManager_allocate)(sizeof(QUEX_NAME(Converter_IConv)),
                                          QUEXED(MemoryObjectType_CONVERTER));

        me->base.open        = (QUEX_NAME(ConverterFunctionP_open))QUEX_NAME(Converter_IConv_open);
        me->base.convert     = (QUEX_NAME(ConverterFunctionP_convert))QUEX_NAME(Converter_IConv_convert);
        me->base.delete_self = (QUEX_NAME(ConverterFunctionP_delete_self))QUEX_NAME(Converter_IConv_delete_self);
        me->base.on_conversion_discontinuity = 0x0;

        me->handle = (iconv_t)-1;

        return (QUEX_NAME(Converter)*)me;
    }

QUEX_NAMESPACE_MAIN_CLOSE


#include <quex/code_base/buffer/BufferFiller.i>

#endif /* __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__ICONV__CONVERTER_ICONV_I */
