Check that a missing library name is correctly detected.
This test includes lib1.gpr, which imports lib2.gpr, and lib1.gpr is invalid.
This ensures that each view’s library name is checked independently
before checking for library name uniqueness.

To be more precise, each view is checked in topological order, so lib2 is
checked before lib1. A bug was that the entire closure of each view was checked
to detect duplicate library names, but accessing an invalid view such as
lib1 before it was validated caused exceptions.