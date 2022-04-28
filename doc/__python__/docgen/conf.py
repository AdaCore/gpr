#!/usr/bin/env python3

import logging

# Substring to escape on Attributes description for sphinx format compliance
substrings_to_escape = ["*"]

# Translation into text from an attribute key and value
attribute_key_value_translation = {
    ("index_type", "NO_INDEX"): "",
    ("index_type", "UNIT_INDEX"): "indexed by a unit",
    ("index_type", "ENV_VAR_NAME_INDEX"): "indexed by an external reference",
    (
        "index_type",
        "FILE_INDEX",
    ): "indexed by a file name",
    ("index_type", "FILEGLOB_INDEX"): "indexed by a source glob",
    ("index_type", "LANGUAGE_INDEX"): "indexed by a language",
    ("index_type", "FILEGLOB_OR_LANGUAGE_INDEX"): (
        "indexed by a source glob or language"
    ),
    ("index_optional", True): '"others" index allowed',
    ("index_optional", False): "",
    ("value", "SINGLE"): "single value",
    ("value", "LIST"): "list value",
    ("value", "SET"): "set value",
    ("value_case_sensitive", True): "case-sensitive",
    ("value_case_sensitive", False): "case-insensitive",
    ("value_is_set", True): "",
    ("value_is_set", False): "",
    ("empty_value", "ALLOW"): "",
    ("empty_value", "IGNORE"): "",
    ("empty_value", "ERROR"): "",
    ("builtin", True): "read-only",
    ("builtin", False): "",
    ("is_allowed_in"): "",
    ("has_default_in"): "",
    ("default"): "",
    ("is_toolchain_config", True): "",
    ("is_toolchain_config", False): "",
    ("config_concatenable", True): "configuration concatenable",
    ("config_concatenable", False): "",
    ("inherit_from_extended", "INHERITED"): "",
    ("inherit_from_extended", "CONCATENATED"): "concatenated with extended value",
    ("inherit_from_extended", "NOT_INHERITED"): "not inherited from extended project",
}

# Complementary files
complementary_files = {
    "Intro_File": "./__python__/metadata/attributes_introduction.rst",
    "Config_File": "./__python__/metadata/attributes_category.json",
}

# General configuration of the documentation generation
display_options = {"Max_Length": 80, "Short_Def": True, "Whole_Def": False}

# Logging configuration
logging_options = {
    "Level": logging.INFO,
    "Format": "[%(levelname)s] - %(message)s",
}
