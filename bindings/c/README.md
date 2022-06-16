# GPR2 C Binding

## General protocol

The GPR2 C Binding is a set of C functions to interact with the GPR2 Ada library.

The binding has only two functions::

    int gpr2_request (int fun_id, char *request, char **answer);

    void gpr2_free_answer (char *answer);

#### gpr2_request

**fun_id** is the function id (see number next to each binding method).

**request** is a JSON string containing one JSON object. The structure of the
object depends on the called method (see documentation of each method).

**answer** is a JSON string containing on JSON object with the following
structure:

    {'result': Dict,
     'status': int,
     'error_msg': str,
     'error_name': str}

If status is set to 0 (OK), then the **result** member contains the return
value. The structure of the returned value is described in each function
If status is set to another value, the call failed. In that case, the
answer object contains the error name and message in **error_msg** and
**error_name**.

In the documentation Python type hinting annotations are used. Note that
in a request, if a JSON object member type is set to Optional it means
that the member is not mandatory in the request and that the default value
is used. For answers optional members are always set. If no default value
is provided then the value of the member is set to null.

### gpr2_free_answer

Memory allocation/deallocation of the request is managed by the caller.
Caller should call **gpr2_free_answer** to release memory allocated for the
answer.

## Common JSON structures

### Source_Location

The following JSON structure is sued for GPR2 source locations:

    {
     'filename': str,
     'line': Optional[int],
     'column': Optional[int]
    }

### Message

The following JSON structure is used for GPR2 messages:

    {
     'level': str,
     'message': str,
     'sloc': Source_Location
    }

### Attribute

The following JSON structures are used for GPR2 attributes depending on
the attribute type. Note that this list may be incomplete.

Single values (e.g. `Library_Name`):

    {
     'value': Optional[str],
     'is_default': bool
    }

List values (e.g. `Source_Dirs`):

    {
     'value': Optional[List[str]],
     'is_default': bool
    }

### Unit

TBD

### Source

TBD

## Operations on Trees

### TREE_LOAD (1)

Loads a project tree.

Request:

     {
      'filename':         str,
      'context':          Optional[Dict[str, str]],
      'build_path':       Optional[str],
      'subdirs':          Optional[str],
      'src_subdirs':      Optional[str],
      'project_dir':      Optional[str],
      'configuration_id': Optional[str],
      'implicit_with':    List[str],
      'target':           Optional[str],
      'runtimes':         Optional[Dict[str, str]],
      'absent_dir_error': Optional[bool] = False,
      'check_shared_lib': Optional[bool] = False
     }

- **filename**: path to the gpr file to load.
- **context**: a JSON object that contains values for external variables
- **build_path**: root directory where build artefacts are stored. If None then
 object, libraries and executable location is relative to the project location.
 this parameter can be used to handled out-of-tree builds
- **subdirs**: If not `null`, then add subdirs as suffix to object, library
and executables directories.
- **src_subdirs**: If not `null`, then add as source directory for each
project the **src_subdirs** subdirectory of the object directory.
- **project_dir**: if defined then load behaves as if filename is located in the
 **project_dir** directory. If `null` directory in which the project file is
 located is used. Using a non `null` allows implementation of "default projects"
 in tools.
- **config**: optional path to a configuration file. If `null` then use the
 default configuration.
- **implicit_with**: optional list of implicitly withed projects
- **target**: optional target name
- **runtimes**: JSON object associating to a language name a runtime name.
- **check_shared_lib**: if True, check in the project tree, that all projects
 describing shared libraries do not import static library or standard project.
- **absent_dir_error**: if True check if some key directories such as object
 dirs do exist

Result:

     {
      'id': str,
      'root_view': str,
      'config_view': Optional[str],
      'runtime_view': Optional[str],
      'target':  str,
      'canonical_target': str,
      'search_paths': List[str],
      'src_subdirs': str,
      'subdirs': str,
      'build_path': str,
      'views': List[str],
      'context': Dict[str, str]
     }

- **id**: the loaded tree id
- **root_view**: view id of the tree root view
- **config_view**: view id of the config view (`null` if there is no configuration)
- **runtime_view**: view id of the runtime view (`null` if there is no runtime view)
- **target**: target as set by the user
- **canonical_target**: normalized target
- **search_paths**: list of search paths used during tree loading
- **src_subdirs**: See equivalent parameter value
- **subdirs**: See equivalent parameter value
- **build_path**: See equivalent parameter value
- **view**: list of view ids sorted topologically
- **context**: tree context

### TREE_UNLOAD (2)

Unload a project tree.

Request:

    {
     'tree_id': str
    }

- **tree_id**: project tree id

Result:

    {}

### TREE_LOG_MESSAGES (3)

Returns log messages associated with a given tree

Request:

    {
     'tree_id':                  str,
     'information':              Optional[bool] = True,
     'warning':                  Optional[bool] = True,
     'error':                    Optional[bool] = True,
     'read':                     Optional[bool] = True,
     'unread':                   Optional[bool] = True
    }

- **tree_id**: gpr2 tree id
- **information**: if True select messages for which level is Information
- **warning**: if True select messages for which level is Warning
- **error**: if True select messages for which level is Error
- **read**: if set to True return already read messages
- **unread**: if set to True return unread messages. Note that all returned
 unread messages are marked read after this call.

Result:

   {
    'messages': List[Message]
   }

### TREE_INVALIDATE_SOURCE_LIST (4)

Invalidate source list and their associated information in the project tree

Request:

    {
     'tree_id': str
    }

- **tree_id**: project tree id

Result:

    {}

### TREE_UPDATE_SOURCE_LIST (5)

Update project tree source list

Request:

    {
     'tree_id': str
    }

- **tree_id**: project tree id

Result:

    {}

### TREE_UPDATE_SOURCE_INFOS (6)

Fetch source informations for all sources in the tree

Request:

    {
     'tree_id': str
    }

- **tree_id**: project tree id

Result:

    {}

## Operations on Views

### VIEW_LOAD (7)

Load a view

Request:

    {
     'tree_id': str
     'view_id': str
    }

Answer:

    {
     'id':   str
     'path': str
     'dir':  str
     'name': str
     'kind': str
    }

### VIEW_ATTRIBUTE (8)

Request:

    {
     'tree_id':  str,
     'view_id':  str,
     'name':     str,
     'pkg':      Optional[str],
     'filename': Optional[str],
     'position': Optional[int],
     'language': Optional[str],
     'index':    Optional[str]
    }


tree_id is the GPR tree id, view_id the GPR view id, name the attribute
name and pkg the optional package name.

filename, position, language and index are used to query an attribute
for a specific index. If the index is a filename use filename and
optionally position (unit index for an Ada source filename). If the
index is a language use 'language'. In all other cases use 'index'.

Answer:

    {
     "attribute": Attribute
    }

### VIEW_SOURCES (9)

Get sources associated with a given view

Request:

    {
     'tree_id': str
     'view_id': str
    }

Answer:

   {
    'sources': list[Source]
   }

### VIEW_UNITS (10)

Get units associated with a given view

Request:

    {
     'tree_id': str
     'view_id': str
    }

Answer:

   {
    'units': list[Unit]
   }

## Operations on Sources

### SOURCE_DEPENDENCIES (11)

Get Source dependencies

Request:

    {
     'tree_id': str,
     'view_id': str,
     'path': str,
     'closure': Optional[bool] = False
    }

### SOURCE_UPDATE_SOURCE_INFOS (12)

Update Source infos (dependencies, ...)

Request:

    {
     'tree_id': str,
     'view_id': str,
     'allow_source_parsing': Optional[bool] = False
    }

