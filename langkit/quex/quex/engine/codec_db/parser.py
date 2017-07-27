from quex.DEFINITIONS                   import QUEX_CODEC_DB_PATH
from quex.engine.misc.interval_handling import NumberSet, Interval
from quex.engine.misc.file_operations   import open_file_or_die
from quex.engine.misc.file_in           import EndOfStreamException, \
                                               read_integer, \
                                               skip_whitespace

def do(section_list, fh):
    """Parses a codec information file. The described codec can only be
    a 'static character length' encoding. That is every character in the
    code occupies the same number of bytes.

    RETURNS: [0] Set of characters in unicode which are covered by the
                 described codec.
             [1] Range of values in the codec elements.
    """
    source_set = NumberSet()
    drain_set  = NumberSet()

    error_str = None

    try:
        while error_str is None:
            skip_whitespace(fh)
            source_begin = read_integer(fh)
            if source_begin is None:
                error_str = "Missing integer (source interval begin) in codec file."
                continue

            skip_whitespace(fh)
            source_size = read_integer(fh)
            if source_size is None:
                error_str = "Missing integer (source interval size) in codec file." 
                continue

            skip_whitespace(fh)
            target_begin = read_integer(fh)
            if target_begin is None:
                error_str = "Missing integer (target interval begin) in codec file."
                continue

            source_end = source_begin + source_size
            list.append(section_list, [source_begin, source_end, target_begin])

            source_set.add_interval(Interval(source_begin, source_end))
            drain_set.add_interval(Interval(target_begin, target_begin + source_size))

    except EndOfStreamException:
        pass

    return source_set, drain_set, error_str

_codec_list_db = []
def get_codec_list_db():
    """
       ...
       [ CODEC_NAME  [CODEC_NAME_LIST]  [LANGUAGE_NAME_LIST] ]
       ...
    """
    global _codec_list_db
    if _codec_list_db: return _codec_list_db

    fh = open_file_or_die(QUEX_CODEC_DB_PATH + "/00-ALL.txt", "rb")
    # FIELD SEPARATOR:  ';'
    # RECORD SEPARATOR: '\n'
    # FIELDS:           [Python Coding Name]   [Aliases]   [Languages] 
    # Aliases and Languages are separated by ','
    _codec_list_db = []
    for line in fh.readlines():
        line = line.strip()
        if len(line) == 0 or line[0] == "#": continue
        fields = map(lambda x: x.strip(), line.split(";"))
        try:
            codec         = fields[0]
            aliases_list  = map(lambda x: x.strip(), fields[1].split(","))
            language_list = map(lambda x: x.strip(), fields[2].split(","))
        except:
            print "Error in line:\n%s\n" % line
        _codec_list_db.append([codec, aliases_list, language_list])

    fh.close()
    return _codec_list_db

