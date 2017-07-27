#! /usr/bin/env python
from   quex.input.files.token_id_file   import TokenInfo, \
                                               space
from   quex.engine.misc.file_in         import get_include_guard_extension
import quex.engine.misc.error           as     error
from   quex.engine.misc.string_handling import blue_print
from   quex.blackboard                  import setup as Setup, \
                                               Lng, \
                                               token_id_db, \
                                               get_used_token_id_set
import quex.blackboard                  as     blackboard

from   collections import defaultdict
from   operator    import attrgetter
import time

standard_token_id_list = ["TERMINATION", "UNINITIALIZED", "INDENT", "NODENT", "DEDENT"]

def do(setup):
    """________________________________________________________________________
       (1) Error Check 
       
       (2) Generates a file containing:
    
       -- token id definitions (if they are not done in '--foreign-token-id-file').

       -- const string& TokenClass::map_id_to_name(), i.e. a function which can 
          convert token ids into strings.
       ________________________________________________________________________
    """
    global file_str
    # At this point, assume that the token type has been generated.
    assert blackboard.token_type_definition is not None

    # (1) Error Check
    #
    __warn_implicit_token_definitions()
    if len(Setup.token_id_foreign_definition_file) == 0:
        __autogenerate_token_id_numbers()
        __warn_on_double_definition()
        # If a mandatory token id is missing, this means that Quex did not
        # properly do implicit token definitions. Program error-abort.
        __error_on_mandatory_token_id_missing(AssertF=True)
    else:
        __error_on_mandatory_token_id_missing()

    __error_on_no_specific_token_ids()

    # (2) Generate token id file (if not specified outside)
    #
    if len(Setup.token_id_foreign_definition_file) != 0:
        # Content of file = inclusion of 'Setup.token_id_foreign_definition_file'.
        token_id_txt = ["#include \"%s\"\n" % Setup.get_file_reference(Setup.token_id_foreign_definition_file)]
    else:
        token_id_txt = __get_token_id_definition_txt()

    include_guard_ext = get_include_guard_extension(Setup.analyzer_name_safe.upper()     \
                                                    + "__"                               \
                                                    + Setup.token_class_name_safe.upper())

    content = blue_print(file_str,
                         [["$$TOKEN_ID_DEFINITIONS$$",        "".join(token_id_txt)],
                          ["$$DATE$$",                        time.asctime()],
                          ["$$TOKEN_CLASS_DEFINITION_FILE$$", Setup.get_file_reference(blackboard.token_type_definition.get_file_name())],
                          ["$$TOKEN_PREFIX$$",                Setup.token_id_prefix], 
                          ["$$INCLUDE_GUARD_EXT$$",           include_guard_ext], 
                         ])

    return content

def do_map_id_to_name_function():
    """Generate function which maps from token-id to string with the 
    name of the token id.
    """
    L = max(map(lambda name: len(name), token_id_db.keys()))

    # -- define the function for token names
    switch_cases = []
    token_names  = []
    for token_name in sorted(token_id_db.keys()):
        if token_name in standard_token_id_list: continue

        # UCS codepoints are coded directly as pure numbers
        if len(token_name) > 2 and token_name[:2] == "--":
            token = token_id_db[token_name]
            switch_cases.append("   case 0x%06X: return token_id_str_%s;\n" % \
                                (token.number, token.name))
            token_names.append("   static const char  token_id_str_%s[]%s = \"%s\";\n" % \
                               (token.name, space(L, token.name), token.name))
        else:
            switch_cases.append("   case %s%s:%s return token_id_str_%s;\n" % \
                                (Setup.token_id_prefix, token_name, space(L, token_name), token_name))
            token_names.append("   static const char  token_id_str_%s[]%s = \"%s\";\n" % \
                               (token_name, space(L, token_name), token_name))

    return blue_print(func_str,
                      [["$$TOKEN_ID_CASES$$", "".join(switch_cases)],
                       ["$$TOKEN_PREFIX$$",   Setup.token_id_prefix], 
                       ["$$TOKEN_NAMES$$",    "".join(token_names)], ])

def prepare_default_standard_token_ids():
    """Prepare the standard token ids automatically. This shall only happen if
    the token ids are not taken from outside, i.e. from a token id file.

    The token ids given here are possibly overwritten later through a 'token'
    section.
    """
    global standard_token_id_list
    assert len(Setup.token_id_foreign_definition_file) == 0

    # 'TERMINATION' is often expected to be zero. The user may still overwrite
    # it, if required differently.
    token_id_db["TERMINATION"] = TokenInfo("TERMINATION", ID=0)
    for name in sorted(standard_token_id_list):
        if name == "TERMINATION": continue 
        token_id_db[name] = TokenInfo(name, ID=__get_free_token_id())

file_str = \
"""/* -*- C++ -*- vim: set syntax=cpp:
 * PURPOSE: File containing definition of token-identifier and
 *          a function that maps token identifiers to a string
 *          name.
 *
 * NOTE: This file has been created automatically by Quex.
 *       Visit quex.org for further info.
 *
 * DATE: $$DATE$$
 *
 * (C) 2005-2010 Frank-Rene Schaefer
 * ABSOLUTELY NO WARRANTY                                           */
#ifndef __QUEX_INCLUDE_GUARD__AUTO_TOKEN_IDS_$$INCLUDE_GUARD_EXT$$__
#define __QUEX_INCLUDE_GUARD__AUTO_TOKEN_IDS_$$INCLUDE_GUARD_EXT$$__

#ifndef __QUEX_OPTION_PLAIN_C
#   include<cstdio> 
#else
#   include<stdio.h> 
#endif

/* The token class definition file can only be included after 
 * the definition on TERMINATION and UNINITIALIZED.          
 * (fschaef 12y03m24d: "I do not rememember why I wrote this.")    */
#include "$$TOKEN_CLASS_DEFINITION_FILE$$"

$$TOKEN_ID_DEFINITIONS$$

QUEX_NAMESPACE_TOKEN_OPEN
extern const char* QUEX_NAME_TOKEN(map_id_to_name)(const QUEX_TYPE_TOKEN_ID TokenID);
QUEX_NAMESPACE_TOKEN_CLOSE

#endif /* __QUEX_INCLUDE_GUARD__AUTO_TOKEN_IDS_$$INCLUDE_GUARD_EXT$$__ */
"""

func_str = \
"""
QUEX_NAMESPACE_TOKEN_OPEN

const char*
QUEX_NAME_TOKEN(map_id_to_name)(const QUEX_TYPE_TOKEN_ID TokenID)
{
   static char  error_string[64];
   static const char  uninitialized_string[] = "<UNINITIALIZED>";
   static const char  termination_string[]   = "<TERMINATION>";
#  if defined(QUEX_OPTION_INDENTATION_TRIGGER)
   static const char  indent_string[]        = "<INDENT>";
   static const char  dedent_string[]        = "<DEDENT>";
   static const char  nodent_string[]        = "<NODENT>";
#  endif
$$TOKEN_NAMES$$       

   /* NOTE: This implementation works only for token id types that are 
    *       some type of integer or enum. In case an alien type is to
    *       used, this function needs to be redefined.                  */
   switch( TokenID ) {
   default: {
       __QUEX_STD_sprintf(error_string, "<UNKNOWN TOKEN-ID: %i>", (int)TokenID);
       return error_string;
   }
   case $$TOKEN_PREFIX$$TERMINATION:    return termination_string;
   case $$TOKEN_PREFIX$$UNINITIALIZED:  return uninitialized_string;
#  if defined(QUEX_OPTION_INDENTATION_TRIGGER)
   case $$TOKEN_PREFIX$$INDENT:         return indent_string;
   case $$TOKEN_PREFIX$$DEDENT:         return dedent_string;
   case $$TOKEN_PREFIX$$NODENT:         return nodent_string;
#  endif
$$TOKEN_ID_CASES$$
   }
}

QUEX_NAMESPACE_TOKEN_CLOSE
"""

def __warn_on_double_definition():
    """Double check that no token id appears twice. Again, this can only happen,
    if quex itself produced the numeric values for the token.

    If the token ids come from outside, Quex does not know the numeric value. It 
    cannot warn about double definitions.
    """
    assert len(Setup.token_id_foreign_definition_file) == 0

    clash_db = defaultdict(list)

    token_list = token_id_db.values()
    for i, x in enumerate(token_list):
        for y in token_list[i+1:]:
            if x.number != y.number: continue
            clash_db[x.number].append(x)
            clash_db[x.number].append(y)

    def find_source_reference(TokenList):
        for token in TokenList:
            if token.sr.is_void(): continue
            return token.sr
        return None
    
    if len(clash_db) != 0:
        item_list = clash_db.items()
        item_list.sort()
        sr = find_source_reference(item_list[0][1])
        error.warning("Following token ids have the same numeric value assigned:", sr)
        for x, token_id_list in item_list:
            sr = find_source_reference(token_id_list)
            token_ids_sorted = sorted(list(set(token_id_list)), key=attrgetter("name")) # Ensure uniqueness
            error.warning("  %s: %s" % (x, "".join(["%s, " % t.name for t in token_ids_sorted])), 
                          sr)
                      
def __warn_implicit_token_definitions():
    """Output a message on token_ids which have been generated automatically.
    That means, that the user may have made a typo.
    """
    if len(blackboard.token_id_implicit_list) == 0: 
        return

    sr  = blackboard.token_id_implicit_list[0][1]
    msg = "Detected implicit token identifier definitions."
    if len(Setup.token_id_foreign_definition_file) == 0:
        msg += " Proposal:\n"
        msg += "   token {"
        error.warning(msg, sr)
        for token_name, sr in blackboard.token_id_implicit_list:
            error.warning("     %s;" % token_name, sr)
        error.warning("   }", sr)
    else:
        error.warning(msg, sr)
        for token_name, sr in blackboard.token_id_implicit_list:
            error.warning("     %s;" % (Setup.token_id_prefix + token_name), sr)
        error.warning("Above token ids must be defined in '%s'" \
                      % Setup.token_id_foreign_definition_file, sr)

def __error_on_no_specific_token_ids():
    all_token_id_set = set(token_id_db.iterkeys())
    all_token_id_set.difference_update(standard_token_id_list)
    if len(all_token_id_set) != 0:
        return

    token_id_str = [
        "    %s%s\n" % (Setup.token_id_prefix, name)
        for name in sorted(token_id_db.iterkeys())
    ]

    error.log("No token id beyond the standard token ids are defined. Found:\n" \
              + "".join(token_id_str) \
              + "Refused to proceed.") 

def __error_on_mandatory_token_id_missing(AssertF=False):
    def check(AssertF, TokenID_Name):
        if AssertF:
            assert TokenID_Name in token_id_db
        elif TokenID_Name not in token_id_db:
            error.log("Definition of token id '%s' is mandatory!" % (Setup.token_id_prefix + TokenID_Name))

    check(AssertF, "TERMINATION")
    check(AssertF, "UNINITIALIZED")
    if blackboard.required_support_indentation_count():
        check(AssertF, "INDENT")
        check(AssertF, "DEDENT")
        check(AssertF, "NODENT")

def __autogenerate_token_id_numbers():
    # Automatically assign numeric token id to token id name
    for dummy, token in sorted(token_id_db.iteritems()):
        if token.number is not None: continue
        token.number = __get_free_token_id()

def __get_token_id_definition_txt():
    
    assert len(Setup.token_id_foreign_definition_file) == 0

    def define_this(txt, token, L):
        assert token.number is not None
        if Setup.language == "C":
            txt.append("#define %s%s %s((QUEX_TYPE_TOKEN_ID)%i)\n" \
                       % (Setup.token_id_prefix_plain, token.name, space(L, token.name), token.number))
        else:
            txt.append("const QUEX_TYPE_TOKEN_ID %s%s%s = ((QUEX_TYPE_TOKEN_ID)%i);\n" \
                       % (Setup.token_id_prefix_plain, token.name, space(L, token.name), token.number))

    if Setup.language == "C": 
        prolog = ""
        epilog = ""
    else:
        prolog = Lng.NAMESPACE_OPEN(Setup.token_id_prefix_name_space)
        epilog = Lng.NAMESPACE_CLOSE(Setup.token_id_prefix_name_space)

    # Considering 'items' allows to sort by name. The name is the 'key' in 
    # the dictionary 'token_id_db'.
    L      = max(map(len, token_id_db.iterkeys()))
    result = [prolog]
    for dummy, token in sorted(token_id_db.iteritems()):
        define_this(result, token, L)
    result.append(epilog)

    return result

def __get_free_token_id():
    used_token_id_set = get_used_token_id_set()
    candidate = Setup.token_id_counter_offset
    while candidate in used_token_id_set:
        candidate += 1
    return candidate

