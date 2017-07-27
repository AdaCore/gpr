import os
import sys
sys.path.append(os.environ["QUEX_PATH"])

from   quex.input.regular_expression.snap_backslashed_character import __parse_hex_number
from   quex.engine.misc.interval_handling    import Interval
from   quex.DEFINITIONS                      import QUEX_CODEC_DB_PATH
from   quex.engine.codec_db.parser           import get_codec_list_db
from   quex.engine.misc.file_operations      import open_file_or_die
import codecs

def __get_transformation(encoder, CharCode):
    # Returns the encoding for the given character code, 
    # plus the number of bytes which it occupies.
    input_str = eval("u'\\U%08X'" % CharCode)
    try:    
        result = encoder(input_str)[0]
    except: 
        # '-1' stands for: 'no encoding for given unicode character'
        return -1, -1

    if len(result) >= 2 and result == "\\u":
        # For compatibility with versions of python <= 2.5, convert the unicode
        # string by hand.
        n = (len(result) - 2) / 2
        return __parse_hex_number(result[2:], len(result) - 2), n

    else:
        L = len(result) 
        if   L == 1: return ord(result), 1
        elif L == 2: return ord(result[0]) * 256      + ord(result[1]), 2
        elif L == 3: return ord(result[0]) * 65536    + ord(result[1]) * 256 + ord(result[2]), 3
        elif L == 4: return ord(result[0]) * 16777216L + ord(result[0]) * 65536 + ord(result[1]) * 256 + ord(result[2]), 4
        else:
            print "Character Encoding of > 4 Bytes."
            return -1, 5

def __create_database_file(TargetEncoding, TargetEncodingName):
    """Writes a database file for a given TargetEncodingName. The 
       TargetEncodingName is required to name the file where the 
       data is to be stored.
    """
    encoder     = codecs.getencoder(TargetEncoding)
    prev_output = -1
    db          = []
    bytes_per_char = -1
    for input in range(0x110000):
        output, n = __get_transformation(encoder, input)

        if bytes_per_char == -1: 
            bytes_per_char = n
        elif n != -1 and bytes_per_char != n:
            print "# not a constant size byte format."
            return False

        # Detect discontinuity in the mapping
        if   prev_output == -1:
            if output != -1:
                input_interval        = Interval(input)
                target_interval_begin = output

        elif output != prev_output + 1:
            # If interval was valid, append it to the database
            input_interval.end    = input
            db.append((input_interval, target_interval_begin))
            # If interval ahead is valid, prepare an object for it
            if output != -1:
                input_interval        = Interval(input)
                target_interval_begin = output

        prev_output = output

    if prev_output != -1:
        input_interval.end = input
        db.append((input_interval, target_interval_begin))

    fh = open_file_or_die(QUEX_CODEC_DB_PATH + "/%s.dat" % TargetEncoding, "wb")
    fh.write("// Describes mapping from Unicode Code pointer to Character code in %s (%s)\n" \
             % (TargetEncoding, TargetEncodingName))
    fh.write("// [SourceInterval.begin] [SourceInterval.Size]  [TargetInterval.begin] (all in hexidecimal)\n")
    for i, t in db:
        fh.write("0x%X %i 0x%X\n" % (i.begin, i.end - i.begin, t))
    fh.close()

    return True

if __name__ == "__main__":
    # PURPOSE: Helper script to create database files that describe the mapping from
    #          unicode characters to character codes of a particular encoding.
    fh           = open("00-ALL.txt")
    fh_supported = open("00-SUPPORTED.txt", "wb")
    # FIELD SEPARATOR:  ';'
    # RECORD SEPARATOR: '\n'
    # FIELDS:           [Python Coding Name]   [Aliases]   [Languages] 
    # Aliases and Languages are separated by ','
    db_list = get_codec_list_db()
    for record in db_list:
        codec         = record[0]
        language_list = record[2]
        print repr(language_list) + " (", codec, ")",
        if __create_database_file(codec, language_list):
            fh_supported.write("%s " % codec)
            print "[OK]"
            
