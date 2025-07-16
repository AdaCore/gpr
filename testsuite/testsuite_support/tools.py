import os

# Environment variable names for each tool
GPRLS_NAME = "GPRLS_NAME"
GPRCLEAN_NAME = "GPRCLEAN_NAME"
GPRINSTALL_NAME = "GPRINSTALL_NAME"
GPRCONFIG_NAME = "GPRCONFIG_NAME"
GPRDOC_NAME = "GPRDOC_NAME"
GPRINSPECT_NAME = "GPRINSPECT_NAME"
GPRBUILD_NAME = "GPRBUILD_NAME"

GPRLS = os.environ[GPRLS_NAME] if GPRLS_NAME in os.environ else "gprls"
GPRCLEAN =  os.environ[GPRCLEAN_NAME] if GPRCLEAN_NAME in os.environ else "gprclean"
GPRINSTALL =  os.environ[GPRINSTALL_NAME] if GPRINSTALL_NAME in os.environ else "gprinstall"
GPRCONFIG =  os.environ[GPRCONFIG_NAME] if GPRCONFIG_NAME in os.environ else "gprconfig"
GPRDOC =  os.environ[GPRDOC_NAME] if GPRDOC_NAME in os.environ else "gprdoc"
GPRINSPECT =  os.environ[GPRINSPECT_NAME] if GPRINSPECT_NAME in os.environ else "gprinspect"
GPRBUILD = os.environ[GPRBUILD_NAME] if GPRBUILD_NAME in os.environ else "gprbuild"
