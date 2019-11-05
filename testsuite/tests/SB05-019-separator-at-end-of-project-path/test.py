import os, subprocess

os.environ['GPR_PROJECT_PATH'] =  os.getenv('GPR_PROJECT_PATH', '') + os.pathsep
os.environ['ADA_PROJECT_PATH'] =  os.getenv('ADA_PROJECT_PATH', '') + os.pathsep

subprocess.check_output('gpr2clean -Pp')