import quex.engine.misc.error as error
import codecs
import os

def open_file_or_die(FileName, Mode="rb", Env=None, CodecCheckF=True):
    fh = __open_safely(FileName, Mode)
    if fh is None:
        if Env is not None:
            error.log("Is environment variable '%s' set propperly?" % Env, 
                      DontExitF=True, NoteF=True)
        error.log("Cannot open file '%s'" % FileName)

    if CodecCheckF and Mode.find("w") == -1: 
        __check_codec(fh, FileName)

    return fh

def get_file_content_or_die(FileName, Mode="rb"):
    fh = open_file_or_die(FileName, Mode)
    txt = fh.read()
    fh.close()
    return txt

def write_safely_and_close(FileName, txt):
    fh = open_file_or_die(FileName, Mode="wb", CodecCheckF=False)
    if os.linesep != "\n": txt = txt.replace("\n", os.linesep)
    # NOTE: According to bug 2813381, maybe due to an error in python,
    #       there appeared two "\r" instead of one "\r\r".
    while txt.find("\r\r") != -1:
        txt = txt.replace("\r\r", "\r")
    fh.write(txt)
    fh.close()

def get_propperly_slash_based_file_name(PathName):
    """Replaces backslashes '\\' by '/' and replaces adjacent
       slashes by single slashes.
    """
    path = PathName.replace("\\", "/")
    while path.find("//") != -1:
        path = path.replace("//", "/")
    return path

# 'Bad' byte order marks (we only treat UTF8)
Bad_BOM_list = [ 
    codecs.BOM_BE, 
    codecs.BOM_LE, 
    codecs.BOM_UTF16, 
    codecs.BOM_UTF16_BE, 
    codecs.BOM_UTF16_LE, 
    codecs.BOM_UTF32, 
    codecs.BOM_UTF32_BE, 
    codecs.BOM_UTF32_LE
]
def __check_codec(fh, FileName):
    global Bad_BOM_list

    begin = fh.read(4)
    for bom in Bad_BOM_list:
        if begin.startswith(bom):
            error.log("Quex requires ASCII or UTF8 input character format.\n" \
                      "File '%s' contains Non-UTF8 Byte Order Mark." % FileName)

    start_pos = 0
    if begin.startswith(codecs.BOM_UTF8):
        start_pos = len(codecs.BOM_UTF8) # Step over the initial BOM

    content = fh.read()
    if content.count('\0') > 1:
        error.log("Quex requires ASCII or UTF8 input character format.\n" \
                  "File '%s' contains many '0' characters. Is it UTF16/UTF32 encoded?." % FileName)

    try: 
        content.decode("utf8")
    except:
        error.log("Quex requires ASCII or UTF8 input character format.\n" \
                  "File '%s' is not ASCII or UTF8 encoded. Or, it contains encoding errors." % FileName)

    fh.seek(start_pos)
    return fh

def __open_safely(FileName, Mode="rb"):
    file_name = FileName.replace("//","/")
    file_name = os.path.normpath(file_name)
    try:
        return open(file_name, Mode)
    except:
        return None

