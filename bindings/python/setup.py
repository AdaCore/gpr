from setuptools import setup, find_packages

import os

# Get e3 version from the VERSION file.
version_file = os.path.join(os.path.dirname(__file__), "VERSION")
with open(version_file) as f:
    gpr2_version = f.read().strip()

with open(os.path.join(os.path.dirname(__file__), "README.md")) as f:
    long_description = f.read()

setup(
    name="gpr2",
    version=gpr2_version,
    url="https://github.com/AdaCore/gpr2",
    license="GPLv3",
    author="AdaCore",
    author_email="info@adacore.com",
    description="Binding to GPR2 library.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    classifiers=[
        "Development Status :: Development Status :: 2 - Pre-Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Topic :: Software Development :: Build Tools",
    ],
    packages=find_packages(where="src"),
    package_data={"gpr2": ["lib/*"]},
    package_dir={"": "src"})

