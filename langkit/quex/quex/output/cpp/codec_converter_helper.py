import os
import sys
sys.path.append(os.environ["QUEX_PATH"])
from copy import copy
from quex.DEFINITIONS                  import QUEX_PATH
from quex.engine.misc.string_handling  import blue_print
from quex.engine.misc.file_in          import make_safe_identifier
from quex.engine.misc.file_operations  import get_file_content_or_die
from quex.engine.codec_db.core         import CodecDynamicInfo
from quex.blackboard                   import setup as Setup, \
                                              Lng

def do():
    if Setup.buffer_codec.name == "unicode": 
        return None, None
    elif isinstance(Setup.buffer_codec, CodecDynamicInfo):
        return None, None

    return _do(Setup.buffer_codec) 

def _do(UnicodeTrafoInfo):
    """
    PURPOSE: Writes converters for conversion towards UTF8/UTF16/UCS2/UCS4.

    UnicodeTrafoInfo:

       Provides the information about the relation of character codes in a particular 
       coding to unicode character codes. It is provided in the following form:

       # Codec Values                 Unicode Values
       [ (Source0_Begin, Source0_End, TargetInterval0_Begin), 
         (Source1_Begin, Source1_End, TargetInterval1_Begin),
         (Source2_Begin, Source2_End, TargetInterval2_Begin), 
         ... 
       ]
    """
    codec_name = make_safe_identifier(UnicodeTrafoInfo.name).lower()
    utf8_epilog,  utf8_function_body  = ConverterWriterUTF8().do(UnicodeTrafoInfo)
    utf16_prolog, utf16_function_body = ConverterWriterUTF16().do(UnicodeTrafoInfo)
    dummy,        utf32_function_body = ConverterWriterUTF32().do(UnicodeTrafoInfo)

    # Provide only the constant which are necessary
    FileName = os.path.normpath(  QUEX_PATH
                                + Lng["$code_base"] 
                                + "/converter_helper/TXT-from-codec-buffer.i")
    codec_header = Setup.get_file_reference(Setup.output_buffer_codec_header)

    txt_i = blue_print(get_file_content_or_die(FileName), 
                       [["$$CODEC$$",        codec_name],
                        ["$$EPILOG$$",       utf8_epilog],
                        ["$$CODEC_HEADER$$", codec_header],
                        ["$$BODY_UTF8$$",    utf8_function_body],
                        ["$$BODY_UTF16$$",   utf16_function_body],
                        ["$$BODY_UTF32$$",   utf32_function_body]])

    # A separate declaration header is required
    FileName = os.path.normpath(  QUEX_PATH
                                + Lng["$code_base"] 
                                + "/converter_helper/TXT-from-codec-buffer")
    template_h_txt = get_file_content_or_die(FileName)
    txt_h = template_h_txt.replace("$$CODEC$$", codec_name)
    return txt_h, txt_i

class ConverterWriter:

    def do(self, UnicodeTrafoInfo, ProvidedConversionInfoF=False):
        """Creates code for a conversion to utf8 according to the conversion_table.
        """
        

        # The flag 'ProvidedConversionTableF' is only to be used for Unit Tests
        if ProvidedConversionInfoF: conversion_table = UnicodeTrafoInfo
        else:                       conversion_table = self.get_conversion_table(UnicodeTrafoInfo)

        # If the converter writer does not do a unicode conversion (even not for range 0),
        # then forget about the bracketing that was done earlier.
        __rely_on_ucs4_conversion_f = (self.get_unicode_range_conversion(conversion_table[0]) == "")

        # Make sure that the conversion table is sorted
        conversion_table.sort(lambda a, b: cmp(a.codec_interval_begin, b.codec_interval_begin))

        # Implement a binary bracketing of conversion domains
        def __bracket(conversion_list, CallerRangeIndex):
            txt = ""
            L   = len(conversion_list)
            if   L == 1:
                txt += self.get_unicode_range_conversion(conversion_list[0])
                # The caller does have to implement an 'encoder
                if CallerRangeIndex != conversion_list[0].byte_format_range_index:
                    txt += self.get_byte_formatter(conversion_list[0].byte_format_range_index)
            else:
                # Determine whether all sub-ranges belong to the same utf8-range
                range_index = self.same_byte_format_range(conversion_list)

                # Bracket interval in the middle
                mid_index = int(float(L)/2)
                Middle    = "0x%06X" % conversion_list[mid_index].codec_interval_begin
                txt += Lng.IF_INPUT("<", Middle) 
                if range_index != -1: 
                    # If there is no 'unicode coversion' and all ranges belong to the 
                    # same byte formatting, then there is no need to bracket further:
                    if not __rely_on_ucs4_conversion_f:
                        txt += __bracket(conversion_list[:mid_index], range_index)
                        txt += Lng.ELSE + "\n"   
                        txt += __bracket(conversion_list[mid_index:], range_index)
                        txt += Lng.END_IF() 
                    if CallerRangeIndex != range_index:
                        txt += self.get_byte_formatter(range_index)
                else:
                    txt += __bracket(conversion_list[:mid_index], range_index)
                    txt += Lng.ELSE + "\n"   
                    txt += __bracket(conversion_list[mid_index:], range_index)
                    txt += Lng.END_IF() 

            if len(txt) != 0 and txt[-1] == "\n": txt = txt[:-1]
            return "    " + txt.replace("\n", "\n    ") + "\n"

        range_index = self.same_byte_format_range(conversion_table)
        txt         = __bracket(conversion_table, range_index)
        if range_index != -1: 
            # All codec ranges belong to the same byte format range.
            formatter_txt = "    " + self.get_byte_formatter(range_index)
            if __rely_on_ucs4_conversion_f:
                txt = formatter_txt
            else:
                txt += formatter_txt[:-1].replace("\n", "\n    ") + "\n"

        return self.get_epilog(), txt

    def get_unicode_range_conversion(self, Info):
        assert isinstance(Info, ConversionInfo)

        # Conversion to Unicode
        offset = Info.codec_interval_begin_unicode - Info.codec_interval_begin
        if offset > 0:   return "unicode = (uint32_t)input + (uint32_t)0x%06X;\n" % offset
        elif offset < 0: return "unicode = (uint32_t)input - (uint32_t)0x%06X;\n" % (-offset)
        else:            return "unicode = (uint32_t)input;\n"
               

    def same_byte_format_range(self, ConvInfoList):
        """RETURNS: >= 0   the common byte format range index.
                    == -1  not all infos belong to the same byte format range.
        """
        range_i = ConvInfoList[0].byte_format_range_index
        for info in ConvInfoList[1:]:
            if info.byte_format_range_index != range_i: return -1
        return ConvInfoList[0].byte_format_range_index

    def get_conversion_table(self, UnicodeTrafoInfo):
        """The UnicodeTrafoInfo tells what ranges in the codec are mapped to what ranges
           in unicode. The codec (e.g. UTF8/UTF16) has ranges of different byte
           formatting. 

           This function identifies ranges in the codec that:

              (1) map linearly to unicode
              (2) belong to the same byte format range.

           The result is a list of objects that identify those ranges in the codec
           and their relation to unicode. See definition of class ConversionInfo
           for a detailed description and a nice picture.
        """
        trafo_info  = copy(UnicodeTrafoInfo)
        border_list = self.get_byte_format_range_border_list()
        L = len(border_list)

        # Sort transform info database according to target range
        info_list = []
        trafo_info.sort(lambda a, b: cmp(a[2], b[2]))

        # Unicode Transformation Info -- A list of the following:
        for source_interval_begin, source_interval_end, target_interval_begin in trafo_info:

            # How does the target interval has to be split according to utf8-ranges?
            i = 0
            while source_interval_begin >= border_list[i]: 
                i += 1

            i -= 1
            # 'i' now stands on the first utf8_range that touches the source interval
            info = ConversionInfo(i, source_interval_begin, target_interval_begin)

            # NOTE: size of target interval = size of source interval
            remaining_size = source_interval_end - source_interval_begin

            ## print "## %i, %x, %x" % (i, source_interval_begin, source_interval_end)
            while i != L - 1 and remaining_size != 0:
                remaining_utf8_range_size = border_list[i+1] - source_interval_begin
                info.codec_interval_size  = min(remaining_utf8_range_size, remaining_size)
                ## print i, "%X: %x, %x" % (border_list[i+1], remaining_utf8_range_size, remaining_size)
                info_list.append(info)

                source_interval_begin  = border_list[i+1] 
                target_interval_begin += info.codec_interval_size
                remaining_size        -= info.codec_interval_size
                i += 1
                info = ConversionInfo(i, source_interval_begin, target_interval_begin)

            if remaining_size != 0:
                info.codec_interval_size = remaining_size
                info_list.append(info)

        info_list.sort(lambda a, b: cmp(a.codec_interval_begin, b.codec_interval_begin))

        return info_list

class ConverterWriterUTF8(ConverterWriter):

    def __init__(self):
        self.range_index_set = set([])

    def get_epilog(self):
        txt = ""
        if 0 in self.range_index_set:
            txt += "one_byte:\n"
            txt += "*((*output_pp)++) = (uint8_t)unicode;\n"
            txt += "return;\n"
    
        if 1 in self.range_index_set:
            txt += "two_bytes:\n"
            txt += "*((*output_pp)++) = (uint8_t)(0xC0 | (unicode >> 6)); \n"
            txt += "*((*output_pp)++) = (uint8_t)(0x80 | (unicode & (uint32_t)0x3f));\n"
            txt += "return;\n"

        if 2 in self.range_index_set:
            txt += "three_bytes:\n"
            txt += "*((*output_pp)++) = (uint8_t)(0xE0 | unicode           >> 12);\n"
            txt += "*((*output_pp)++) = (uint8_t)(0x80 | (unicode & (uint32_t)0xFFF) >> 6);\n"
            txt += "*((*output_pp)++) = (uint8_t)(0x80 | (unicode & (uint32_t)0x3F));\n"
            txt += "return;\n"

        if 3 in self.range_index_set:
            txt += "four_bytes:\n"
            txt += "/* Assume that only character appear, that are defined in unicode. */\n"
            txt += "__quex_assert(unicode <= (uint32_t)0x1FFFFF);\n"
            txt += "/* No surrogate pairs (They are reserved even in non-utf16).       */\n"
            txt += "__quex_assert(! (unicode >= 0xd800 && unicode <= 0xdfff) );\n"

            txt += "*((*output_pp)++) = (uint8_t)(0xF0 | unicode >> 18);\n"
            txt += "*((*output_pp)++) = (uint8_t)(0x80 | (unicode & (uint32_t)0x3FFFF) >> 12);\n"
            txt += "*((*output_pp)++) = (uint8_t)(0x80 | (unicode & (uint32_t)0xFFF)   >> 6);\n"
            txt += "*((*output_pp)++) = (uint8_t)(0x80 | (unicode & (uint32_t)0x3F));\n"

        return txt

    def get_byte_formatter(self, RangeIndex):
        assert RangeIndex >= 0 
        assert RangeIndex <= 3
        # Byte Split
        self.range_index_set.add(RangeIndex)
        return {
            0: "goto one_byte;\n",
            1: "goto two_bytes;\n",
            2: "goto three_bytes;\n",
            3: "goto four_bytes;\n",
        }[RangeIndex] 

    def get_byte_format_range_border_list(self):
        """UTF8 covers the following regions with the corresponding numbers of bytes:
        
             0x00000000 - 0x0000007F: 1 byte  - 0xxxxxxx
             0x00000080 - 0x000007FF: 2 bytes - 110xxxxx 10xxxxxx
             0x00000800 - 0x0000FFFF: 3 bytes - 1110xxxx 10xxxxxx 10xxxxxx
             0x00010000 - 0x001FFFFF: 4 bytes - 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
             0x00200000 - 0x03FFFFFF: 5 bytes ... (not for unicode)
             0x04000000 - 0x7FFFFFFF: 

            The range borders are, therefore, as mentioned in the return value.
        """
        return [ 0x0, 0x00000080, 0x00000800, 0x00010000, 0x00200000, 0x04000000, 0x80000000, sys.maxint] 

class ConverterWriterUTF16(ConverterWriter):
    def get_epilog(self):
        return ""

    def get_byte_formatter(self, RangeIndex):
        return { 0: "*(*output_pp)++ = unicode;\n",
                 1: "const uint16_t Offset_10bit_high = (uint16_t)((unicode - 0x10000) >> 10);\n"  + \
                    "const uint16_t Offset_10bit_low  = (uint16_t)((unicode - 0x10000) & 0x3FF);\n" + \
                    "*(*output_pp)++ = 0xD800 | offset_10bit_high;\n"
                    "*(*output_pp)++ = 0xDC00 | offset_10bit_low;\n",
                    }[RangeIndex]

    def get_byte_format_range_border_list(self):
        """UCS4 covers the whole range of unicode (extend 0x10FFFF to sys.maxint to be nice)."""
        return [ 0x0, 0x10000, sys.maxint] 
    
    def get_unicode_range_conversion(self, Info):
        # Take the unicode value via the UCS4 converter
        return ""

class ConverterWriterUTF32(ConverterWriter):
    def get_epilog(self):
        return ""

    def get_byte_formatter(self, RangeIndex):
        return "*(*output_pp)++ = unicode;\n"

    def get_byte_format_range_border_list(self):
        """UCS4 covers the whole range of unicode (extend 0x10FFFF to sys.maxint to be nice)."""
        return [ 0x0, sys.maxint] 

class ConversionInfo:
    """A given interval in the codec corresponds to a certain byte formatting
       range in the target encoding, where all bytes are formatted the same 
       way.
         
         -- The codec interval is determined by:      
              .codec_interval_begin
              .codec_interval_size
         
         -- The byte formatting range is determined by its index.
              .byte_format_range_index

         -- In order to know where to start, the unicode offset that corresponds 
            to the codec interval must be specified:
              .codec_interval_begin_unicode

       Figure:

              Source Codec
                              ci_begin       
                              |
              ................[xxxxxxxxxxxxxxx]................
                              |--- ci_size -->|


              belongs to


              Unicode    |<----          byte formatting range         ---->|
                                                                             
                         |                           |--- ci_size-->|       |
              ...........[+++++++++++++++++++++++++++|xxxxxxxxxxxxxx|++++++][
                                                     |
                                                     ci_begin_unicode
                                                      

       The codec interval always lies inside a single utf8 range.
    """
    def __init__(self, RangeIndex, CI_Begin_in_Unicode, CI_Begin, CI_Size=-1):
        self.byte_format_range_index      = RangeIndex
        self.codec_interval_begin         = CI_Begin
        self.codec_interval_size          = CI_Size
        self.codec_interval_begin_unicode = CI_Begin_in_Unicode

    def __repr__(self):
        return "[%i] at %08X: Codec Interval [%X,%X)" % \
               (self.byte_format_range_index,
                self.codec_interval_begin_unicode,
                self.codec_interval_begin,
                self.codec_interval_begin + self.codec_interval_size)

