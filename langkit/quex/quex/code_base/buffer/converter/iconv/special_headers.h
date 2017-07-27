/* -*- C++ -*- vim: set syntax=cpp: 
 *
 * PURPOSE:
 *   
 *   Quex allows to connect to different unicode converters. Not all of those converters
 *   are necessarily installed on every system. However, each converter library provides
 *   header files which need to be included to use the library. There must be a mechanism
 *   to prevent the inclusion of converter headers that the user does not provide.
 *   
 *   In quex the usage of a particular converter is 'announced' via a macro, e.g.
 *   if QUEX_OPTION_CONVERTER_ICONV is defined it means that the ICONV library is used for 
 *   conversion and the correspondent headers are to be used. 
 *   
 *   If this macro is not defined, then the compiler should never reach this file--
 *   and this case is caught by the first section of this file.                            
 *
 *   (C) 2009 Frank-Rene Schaefer
 *
 *   ABSOLUTELY NO WARRANTY                                                                 */
#ifndef __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__ICONV__SPECIAL_HEADERS_H
#define __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__ICONV__SPECIAL_HEADERS_H

#if ! defined(QUEX_OPTION_CONVERTER_ICONV)
#    error "This header has been included without setting the compile option QUEX_OPTION_CONVERTER_ICONV. This could cause problems on systems where the correspondent headers are not installed. Make the inclusion of this header dependent on the above compile option."
#endif

#if ! defined (__QUEX_OPTION_PLAIN_C)
    extern "C" { 
#   include <iconv.h>
    }
#else
#   include <iconv.h>
#endif

#endif /* __QUEX_INCLUDE_GUARD__BUFFER__CONVERTER__ICONV__SPECIAL_HEADERS_H */
