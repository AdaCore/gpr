from   quex.engine.misc.tools      import typed
import quex.engine.misc.similarity as     similarity
from   quex.input.code.base        import SourceRef, SourceRef_VOID

from   StringIO import StringIO
import types
import sys
import os

__reference_to_setup = None
def specify_setup_object(TheSetup):
    global __reference_to_setup 
    __reference_to_setup = TheSetup

@typed(Fh_or_Sr=(types.IntType, SourceRef, types.FileType, StringIO), Prefix=str)
def warning(ErrMsg, Fh_or_Sr, Prefix="", SuppressCode=None):
    log(ErrMsg, Fh_or_Sr, DontExitF=True, WarningF=True, NoteF=False, SuppressCode=SuppressCode)

@typed(Fh_or_Sr=(types.IntType, SourceRef, types.FileType, StringIO), DontExitF=bool)
def log(ErrMsg, Fh_or_Sr=-1, DontExitF=False, Prefix="", WarningF=False, NoteF=False, SuppressCode=None):
    # Fh_or_Sr        = filehandle [1] or filename [2]
    # LineN     = line_number of error
    # DontExitF = True then no exit from program
    #           = False then total exit from program
    # WarningF  = Asked only if DontExitF is set. 
    #
    # count line numbers (this is a kind of 'dirty' solution for not
    # counting line numbers on the fly. it does not harm at all and
    # is much more direct to be programmed.)
    global __reference_to_setup

    PlainMessageF = Fh_or_Sr is None

    if     __reference_to_setup is not None \
       and SuppressCode in __reference_to_setup.suppressed_notification_list:
        return

    if NoteF: DontExitF = True

    if Fh_or_Sr == -1:
        sr = SourceRef_VOID
    elif isinstance(Fh_or_Sr, SourceRef):
        sr = Fh_or_Sr
    elif isinstance(Fh_or_Sr, (types.FileType, StringIO)):
        sr = SourceRef.from_FileHandle(Fh_or_Sr)
    else:
        assert False

    if Fh_or_Sr == -1:
        if Prefix == "": prefix = "command line"
        else:            prefix = Prefix
    elif PlainMessageF:  prefix = "message"
    elif NoteF:          prefix = "%s:%i:note"    % (sr.file_name, sr.line_n)   
    elif WarningF:       prefix = "%s:%i:warning" % (sr.file_name, sr.line_n)   
    else:                prefix = "%s:%i:error"   % (sr.file_name, sr.line_n)   
        
    if ErrMsg:
        for line in ErrMsg.splitlines():
            print prefix + ": %s" % line

    if SuppressCode is not None:
        print prefix + ": ('--suppress %s' to avoid this message)" % SuppressCode

    if not DontExitF: sys.exit(-1)  # Here, sys.exit(-1) is accepted

def error_eof(title, fh):
    log("End of file reached while parsing '%s' section." % title, fh)

def log_file_not_found(FileName, Comment="", FH=-1):
    """FH and LineN follow format of 'log(...)'"""
    directory = os.path.dirname(FileName)
    if directory == "": directory = os.path.normpath("./"); suffix = "."
    else:               suffix = " in directory\n'%s'." % directory
    comment = ""
    if Comment != "": comment = "(%s) " % Comment
    try:
        ext = os.path.splitext(FileName)[1]

        files_in_directory = [
            file for file in os.listdir(directory) 
            if file.rfind(ext) == len(file) - len(ext)
        ]
    except:
        log("File '%s' (%s) not found." % (FileName, comment), FH)

    verify_word_in_list(FileName, files_in_directory, 
                        "File '%s' %snot found%s" % (FileName, comment, suffix), FH)
    
@typed(Fh_or_Sr=(types.IntType, SourceRef, types.FileType, StringIO))
def verify_word_in_list(Word, WordList, Comment, Fh_or_Sr=-1, ExitF=True, SuppressCode=None):
    """FH, and LineN work exactly the same as for error.log(...)"""
    if     __reference_to_setup is not None \
       and SuppressCode in __reference_to_setup.suppressed_notification_list:
        return

    if not WordList:
        log(Comment + "\n'%s' is not addmissible here." % Word, Fh_or_Sr, DontExitF=False, 
            SuppressCode=SuppressCode)
        return

    position_known_f = False
    if type(WordList) == dict:
        word_list = WordList.keys()
    elif WordList[0].__class__.__name__ == "UserCodeFragment":
        word_list        = map(lambda x: x.get_code(), WordList)
        position_known_f = True
    else:
        word_list        = WordList

    if Word in word_list: return True

    # Word was not in list
    similar_index = similarity.get(Word, word_list)

    if similar_index == -1:
        txt = "Acceptable: "
        length = len(txt)
        for word in WordList:
            L = len(word)
            if length + L > 80: 
                txt += "\n"; length = 0
            txt += word + ", "
            length += L

        if txt != "": txt = txt[:-2] + "."
        log(Comment + "\n" + txt, Fh_or_Sr, DontExitF=False,
            SuppressCode=SuppressCode)

    else:
        similar_word = word_list[similar_index]
        if position_known_f:
            log(Comment, Fh_or_Sr, DontExitF=True)
            log("Did you mean '%s'?" % similar_word,
                WordList[similar_index].sr, 
                DontExitF=not ExitF, 
                SuppressCode=SuppressCode)
        else:
            log(Comment + "\n" + "Did you mean '%s'?" % similar_word,
                Fh_or_Sr, DontExitF=not ExitF, 
                SuppressCode=SuppressCode)

    return False


