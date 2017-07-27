from   quex.input.setup                 import NotificationDB
from   quex.input.code.base             import SourceRef, \
                                               SourceRef_VOID
import quex.engine.misc.error           as     error
from   quex.engine.misc.file_operations import get_file_content_or_die, \
                                               open_file_or_die
from   quex.engine.misc.file_in         import delete_comment
from   quex.blackboard                  import setup as Setup, \
                                               token_id_db, \
                                               token_id_foreign_set
from   itertools import chain
from   copy      import copy
import re
import os

class TokenInfo:
    def __init__(self, Name, ID, TypeName=None, SourceReference=SourceRef_VOID):
        self.name         = Name
        self.number       = ID
        self.related_type = TypeName
        self.id           = None
        self.sr           = SourceReference

def parse(ForeignTokenIdFile, CommentDelimiterList):
    """This function somehow interprets the user defined token id file--if there is
       one. It does this in order to find the names of defined token ids. It does
       some basic interpretation and include file following, but: **it is in no
       way perfect**. Since its only purpose is to avoid warnings about token ids
       that are not defined it is not essential that it may fail sometimes.

       It is more like a nice feature that quex tries to find definitions on its own.
       
       Nevertheless, it should work in the large majority of cases.
    """
    # Regular expression to find '#include <something>' and extract the 'something'
    # in a 'group'. Note that '(' ')' cause the storage of parts of the match.
    IncludeRE = "#[ \t]*include[ \t]*[\"<]([^\">]+)[\">]"

    include_re_obj = re.compile(IncludeRE)

    def get_line_n_of_include(FileName, IncludedFileName):
        fh = open_file_or_die(FileName, Mode="rb")
        line_n = 0
        for line in fh.readlines():
            line_n += 1
            if include_re_obj.search(line) is not None and line.find(IncludedFileName) != -1:
                break
        else:
            # Included file must appear in including file, but tolerate for safety.
            pass

        fh.close()
        return line_n

    # validate(...) ensured, that the file exists.
    work_list      = [ ForeignTokenIdFile ] 
    done_list      = []
    not_found_list = []
    recursive_list = []
    found_db       = {}
    while len(work_list) != 0:
        file_name = work_list.pop()
        content   = __delete_comments(get_file_content_or_die(file_name, Mode="rb"), 
                                      CommentDelimiterList)
        done_list.append(os.path.normpath(file_name))

        # (*) Search for TokenID definitions 
        begin_i = 0
        end_i   = len(content)
        if Setup.token_id_foreign_definition_file_region_begin_re is not None:
            match = Setup.token_id_foreign_definition_file_region_begin_re.search(content)
            if match is not None:
                begin_i = match.end()

        if Setup.token_id_foreign_definition_file_region_end_re is not None:
            match = Setup.token_id_foreign_definition_file_region_end_re.search(content, pos=begin_i)
            if match is not None:
                end_i = match.start()
        content = content[begin_i:end_i]

        token_id_list = __extract_token_ids(content, file_name)
        if len(token_id_list) != 0:
            found_db[file_name] = copy(token_id_list)

        token_id_foreign_set.update(token_id_list)
        for token_name in token_id_list:
            # NOTE: The line number might be wrong, because of the comment deletion
            line_n = 0
            # NOTE: The actual token value is not important, since the token's numeric
            #       identifier is defined in the user's header. We do not care.
            prefix_less_token_name = cut_token_id_prefix(token_name)
            token_id_db[prefix_less_token_name] = \
                        TokenInfo(prefix_less_token_name, None, None, SourceRef(file_name, line_n)) 
        
        # (*) find "#include" statements
        #     'set' ensures that each entry is unique
        include_file_set = set(include_re_obj.findall(content))

        #     -- ensure that included files exist and are not included twice
        for included_file in include_file_set:
            normed_included_file = os.path.normpath(included_file)
            if included_file in done_list:
                line_n = get_line_n_of_include(file_name, included_file)
                recursive_list.append((file_name, line_n, included_file))
            elif not os.access(normed_included_file, os.F_OK): 
                line_n = get_line_n_of_include(file_name, included_file)
                not_found_list.append((file_name, line_n, included_file))
            elif normed_included_file not in done_list:
                work_list.append(included_file)

    if Setup.token_id_foreign_definition_file_show_f:
        if len(found_db) == 0:
            error.log(  "No token ids with prefix '%s' found in" % Setup.token_id_prefix
                      + "'%s' or included files." % Setup.token_id_foreign_definition_file, 
                     NoteF=True)
        else:
            txt = [] 
            for file_name, result in found_db.iteritems():
                result = set(result)
                L = max(map(len, result))
                txt.append("Token ids found in file '%s' {\n" % file_name)
                for name in sorted(result):
                    shorty = cut_token_id_prefix(name)
                    fully  = Setup.token_id_prefix + shorty
                    txt.append("     %s %s=> '%s'\n" % (fully, space(L, name), shorty))
                txt.append("}")
                txt.append("\n")

            if txt: txt = txt[:-1]
            error.log("".join(txt), NoteF=True)
            
    ErrorN = NotificationDB.token_id_ignored_files_report
    if ErrorN not in Setup.suppressed_notification_list:
        if not_found_list:
            not_found_list.sort()
            sr = SourceRef(not_found_list[0][0], LineN=not_found_list[0][1]) 
            error.warning("Files not found:", sr)
            for file_name, line_n, included_file in not_found_list:
                error.warning("%s" % included_file, SourceRef(file_name, line_n))

        if recursive_list:
            recursive_list.sort()
            sr = SourceRef(recursive_list[0][0], LineN=recursive_list[0][1]) 
            error.warning("Files recursively included (ignored second inclusion):", 
                          sr)
            for file_name, line_n, included_file in recursive_list:
                error.warning("%s" % included_file, SourceRef(file_name, line_n))

        if not_found_list or recursive_list:
            # source reference is taken from last setting
            error.log("\nQuex does not handle C-Preprocessor instructions.",
                      sr, NoteF=True, DontExitF=True, SuppressCode=ErrorN)

def cut_token_id_prefix(TokenName, FH_Error=False):
    if TokenName.find(Setup.token_id_prefix) == 0:
        return TokenName[len(Setup.token_id_prefix):]
    elif TokenName.find(Setup.token_id_prefix_plain) == 0:
        return TokenName[len(Setup.token_id_prefix_plain):]
    elif not FH_Error:
        return TokenName
    else:
        error.log("Token identifier does not begin with token prefix '%s'\n" % Setup.token_id_prefix + \
                  "found: '%s'" % TokenName, FH_Error)

def __delete_comments(Content, CommentDelimiterList):
    content = Content
    for opener, closer in CommentDelimiterList:
        content = delete_comment(content, opener, closer, LeaveNewlineDelimiter=True)
    return content

def __extract_token_ids(PlainContent, FileName):
    """PlainContent     -- File content without comments.
    """
    DefineRE      = "#[ \t]*define[ \t]+([^ \t\n\r]+)[ \t]+[^ \t\n]+"
    AssignRE      = "([^ \t]+)[ \t]*=[ \t]*[^ \t]+"
    EnumRE        = "enum[^{]*{([^}]*)}"
    EnumConst     = "([^=, \n\t]+)"
    define_re_obj = re.compile(DefineRE)
    assign_re_obj = re.compile(AssignRE)
    enum_re_obj   = re.compile(EnumRE)
    const_re_obj  = re.compile(EnumConst)

    def check_and_append(found_list, Name):
        if    len(Setup.token_id_prefix_plain) == 0 \
           or Name.find(Setup.token_id_prefix_plain) == 0 \
           or Name.find(Setup.token_id_prefix) == 0:
            found_list.append(Name)

    result = []
    for name in chain(define_re_obj.findall(PlainContent), 
                      assign_re_obj.findall(PlainContent)):
        # Either there is no plain token prefix, or it matches well.
        check_and_append(result, name)

    for enum_txt in enum_re_obj.findall(PlainContent):
        for name in const_re_obj.findall(enum_txt):
            check_and_append(result, name.strip())

    return result

def space(L, Name):
    return " " * (L - len(Name))

