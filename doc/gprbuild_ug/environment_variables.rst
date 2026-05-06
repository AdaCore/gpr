.. index:: environment variable

.. _Environment_Variables:

Environment Variables
---------------------

Project processing can be affected by environment variables.

* **GPR_CONFIG**

  When declared with a non empty name, use its value as the default
  configuration project file name in native platforms, instead of
  :file:`default.cgpr`.

* **GPR_RUNTIME_PATH**

  Path where to look for a non empty runtime directory.

* **GPR_REGISTRY_DIRS**

  The paths where to look for the registry files. Each path is separated by a
  colon (``:``) in Unix and a semicolon (``;``) in Windows. All files with a
  ``.json`` extension in these directories are considered as registry files.

* **PATH**

  The path, that may be modified to add the directories related to the
  compilers.

* **GPR_PROJECT_PATH_FILE**

  The path to a file containing project directory path names

* **GPR_PROJECT_PATH**

  The path where to look for projects

* **ADA_PROJECT_PATH**

  The path where to look for projects

* **TMPDIR**

  Directories where to create temporary files

* **TEMP**

  Directories where to create temporary files

* **TMP**

  Directories where to create temporary files

* **GPR_VERBOSITY**

  Value for the quiet mode or the verbosity level.
  Overriden with switches ``-q``, ``-v`` and ``-vP?``

* **GPRBIND_DEBUG**

  When value is "TRUE", keep a copy of the binder exchange file sent to
  gprbind as :file:`main.bexch__saved`

* **GPRLIB_DEBUG**

  When value is "TRUE", keep a copy of the library exchange file sent to
  gprlib as :file:`main.lexch__saved`
