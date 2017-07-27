/* -*- C++ -*-  vim: set syntax=cpp:
 * (C) 2007-2008 Frank-Rene Schaefer  */
#ifndef __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__RECODE__CONVERTER_RECODE_I
#define __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__RECODE__CONVERTER_RECODE_I

#include <quex/code_base/compatibility/stdint.h>
#include <quex/code_base/MemoryManager>

#if ! defined(QUEX_OPTION_CONVERTER_ICU)
#    error "This header has been included without setting the compile option QUEX_OPTION_CONVERTER_ICU. This could cause problems on systems where the correspondent headers are not installed. Make the inclusion of this header dependent on the above compile option."
#endif

QUEX_NAMESPACE_MAIN_OPEN

    QUEX_INLINE void
    QuexConverter_ICU_open(QuexConverter* alter_ego, 
                           const char* FromCodingName, const char* ToCodingName)
    {
        QuexConverter_ICU* me = (QuexConverter_ICU*)alter_ego;
        __quex_assert(me != 0x0);

        me->from_handle = ucnv_open(FromCodingName, &me->status);

        if( ! U_SUCCESS(me->status) ) 
            QUEX_ERROR_EXIT("Input Coding not supported by ICU converter.");

        if( ToCodingName != 0x0 ) {
            me->to_handle = ucnv_open(ToCodingName, &me->status);
        } else {
            switch( sizeof(QUEX_TYPE_CHARACTER) ) {
            case 4:  
                me->to_handle = ucnv_open("UTF32_PlatformEndian", &me->status); 
                break;
            case 2:  
                me->to_handle = 0x0; /* 2 byte encoding may use the 'direct converter for UChar' */
                break;
            case 1:  
                me->to_handle = ucnv_open("ISO-8859-1", &me->status); 
                break;
            default:
                QUEX_ERROR_EXIT("ICU character conversion: target coding different from unicode not yet supported.");
            }
        }

        me->pivot_iterator_begin = me->pivot_buffer;
        me->pivot_iterator_end   = me->pivot_buffer;
    }

    QUEX_INLINE bool
    QuexConverter_ICU_convert(QuexConverter*        alter_ego, 
                              uint8_t**             source, const uint8_t*              SourceEnd, 
                              QUEX_TYPE_CHARACTER** drain,  const QUEX_TYPE_CHARACTER*  DrainEnd)
    {
        /* RETURNS: 'true'  if the drain was completely filled.
         *          'false' if the drain could not be filled completely and more source
         *                  bytes are required.                                          */
        QuexConverter_ICU*    me         = (QuexConverter_ICU*)alter_ego;

        __quex_assert(me != 0x0);
        me->status = U_ZERO_ERROR;

        if( me->to_handle == 0x0 ) {
            /* Convert according to QUEX_TYPE_CHARACTER:
             *
             * NOTE: The author did not find a better way to do non-16bit conversion than
             *       converting 'normally' and then shifting according to the size
             *       of QUEX_TYPE_CHARACTER. If you read these lines and know of a better
             *       method, please, let me know (email: fschaef@users.sourceforge.net).   
             *
             * NOTE: 'UChar' is defined to be wchar_t, if sizeof(wchar_t) == 2 byte, 
             *       otherwise it as defined as uint16_t.                        
             *
             * We need to cast to UChar, since otherwise the code would not compile for sizeof() != 2.
             * Nevertheless, in this case the code would never be executed.                            */
            __quex_assert( sizeof(QUEX_TYPE_CHARACTER) == 2 );

            /* 16 bit --> nothing to be done */
            ucnv_toUnicode(me->from_handle, 
                           (UChar**)drain,       (const UChar*)DrainEnd,
                           (const char**)source, (const char*)SourceEnd, 
                           /* offsets */NULL,
                           /* flush = */FALSE,
                           &me->status);

            if( *drain == DrainEnd ) return true;
            else                     return false;

        } else {
            ucnv_convertEx(me->to_handle, me->from_handle,
                           (char**)drain,        (const char*)DrainEnd,
                           (const char**)source, (const char*)SourceEnd,
                           me->pivot_buffer, 
                           &me->pivot_iterator_begin, &me->pivot_iterator_end, 
                           me->pivot_buffer + QUEX_SETTING_ICU_PIVOT_BUFFER_SIZE,
                           /* reset = */FALSE, 
                           /* flush = */FALSE,
                           &me->status);

            if( *drain == DrainEnd ) return true;
            else                     return false;
        }

        /*
        if( me->status == U_BUFFER_OVERFLOW_ERROR) {
            return false;
        }
        else {
            if( ! U_SUCCESS(me->status) ) {
                QUEX_ERROR_EXIT(u_errorName(me->status));
            }
            / * Are more source bytes needed to fill the drain buffer? If so we return 'false' * /
            if( *drain != DrainEnd && *source == SourceEnd ) return false;
            else                                             return true;
        }
        */
    }

    QUEX_INLINE void 
    QuexConverter_ICU_on_conversion_discontinuity(QuexConverter* alter_ego)
    {
        QuexConverter_ICU* me = (QuexConverter_ICU*)alter_ego;

        ucnv_reset(me->from_handle);
        if( me->to_handle != 0x0 ) ucnv_reset(me->to_handle);

        /* Reset the pivot buffer iterators */
        me->pivot_iterator_begin = me->pivot_buffer;
        me->pivot_iterator_end   = me->pivot_buffer;

        me->status = U_ZERO_ERROR;
    }

    QUEX_INLINE void
    QuexConverter_ICU_delete_self(QuexConverter* alter_ego)
    {
        QuexConverter_ICU* me = (QuexConverter_ICU*)alter_ego;

        ucnv_close(me->from_handle);
        ucnv_close(me->to_handle);

        QUEXED(MemoryManager_free)((void*)me, QUEXED(MemoryObjectType_CONVERTER));

        /* There should be a way to call 'ucnv_flushCache()' as soon as all converters
         * are freed automatically.                                                       */
    }

    QUEX_INLINE QuexConverter*
    QuexConverter_ICU_new()
    {
        QuexConverter_ICU*  me = \
             (QUEX_NAME(Converter_ICU)*)QUEXED(MemoryManager_allocate)(sizeof(QUEX_NAME(Converter_ICU)),
                                                                       QUEXED(MemoryObjectType_CONVERTER));

        me->base.open        = QuexConverter_ICU_open;
        me->base.convert     = QuexConverter_ICU_convert;
        me->base.delete_self = QuexConverter_ICU_delete_self;
        me->base.on_conversion_discontinuity  = QuexConverter_ICU_on_conversion_discontinuity;

        me->to_handle   = 0x0;
        me->from_handle = 0x0;
        me->status      = U_ZERO_ERROR;

        return (QuexConverter*)me;
    }

QUEX_NAMESPACE_MAIN_CLOSE


#include <quex/code_base/buffer/BufferFiller.i>

#endif /* __INCLUDE_GUARD__QUEX_BUFFER__CONVERTER_ICONV_I__ */
