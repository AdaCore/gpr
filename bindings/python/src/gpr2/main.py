#
#  Copyright (C) 2022, AdaCore
#
#  SPDX-License-Identifier: Apache-2.0
#

from __future__ import annotations


def add_project_options(arg_parser) -> None:
    """Add to an argument parser switches needed to load a project."""
    arg_parser.add_argument("--project", "-P", metavar="FILE", help="Project filename")
    arg_parser.add_argument("--target", help="Specify a target for cross platforms")
    arg_parser.add_argument(
        "--config", metavar="FILE", help="Specify the main config project file name"
    )
    arg_parser.add_argument(
        "--autoconf",
        metavar="FILE",
        help="Specify/create the main config project file name",
    )
    arg_parser.add_argument(
        "--implicit-with",
        metavar="FILE",
        action="append",
        help="Add the given projects as a dependency on all loaded projects",
    )
    arg_parser.add_argument(
        "--src-subdirs",
        metavar="DIR",
        help="Prepend <obj>/dir to the list of source dirs for each project",
    )
    arg_parser.add_argument(
        "--subdirs", metavar="DIR", help="Use dir as suffix to obj/lib/exec directories"
    )
    arg_parser.add_argument(
        "--RTS",
        metavar="[LANGUAGE:]RUNTIME",
        help="Use runtime RUNTIME for language LANGUAGE "
        "(if LANGUAGE is not specified then Ada is assumed)",
    )
    arg_parser.add_argument(
        "-X",
        dest="context",
        metavar="KEY=VALUE",
        action="append",
        help="Specify an external reference for project Files",
    )
