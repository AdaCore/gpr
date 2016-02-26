GPR2 testsuite
##############

How to run the testsuite
========================

First of all, make sure you have installed all the dependencies in your
environment:

* built the GPR2 library;
* installed the runtime dependencies (Libiconv, GNATcoll) in your environment;
* installed the GNATpython package, required by the testsuite itself.

Then, in the ``testsuite/`` directory, run:

.. code-block:: text

    ./testsuite.py

This will run all the testcases (showing progression on standard output) and
put a report in the ``out/`` directory. Note that you can actually run the
testsuite from whatever directory that suits you: in any case, it will create
the ``out/`` report in the current directory.

Depending on your needs, you may want to run only specific testcases. This is
easy: just pass these testcases as arguments to the ``testsuite.py`` script.
For instance:

.. code-block:: text

    ./testsuite.py tests/foo tests/bar

... will run all the testcases present below the ``tests/foo`` and ``test/bar``
directories, but not the ones in ``tests/foobar``.

The testsuite also has several useful options to alter how it runs. For
instance: ``--show-error-output`` will display the difference between expected
output and actual one on standard output. You can discover these options with
``testsuite.py``'s ``--help`` flag.


How to write new testcases
==========================

Basics
------

Create a new directory in the ``tests/`` folder (it can be in subdirectories
itself). What will make it a testcase is to write a ``test.yaml`` file in it.
This YAML file describes to the testsuite framework how the test should be run
and can provide additional metadata such as a description for the testcase.
Currently, you only have to define two fields, that are mandatory:

* ``description``, a short description for this testcase to make it
  clear what's the motivation behind it, what makes it relevant next to other
  testcases, etc.

* ``driver``, a single identifier which describes what engine will run the
  testcase. Unlike fixedbugs-style testsuites, in which the driver is the
  ``test.{cmd,sh}`` script, ``gnatpyton.testsuite``-style testsuites keep
  test logic and testcase data separate. Test logic stays in the testsuite
  framework and the various "test patterns" are called *drivers*. On the other
  hand, testcases provide data the driver uses to run the test and decide
  whether it passed or not.

  There are two drivers available currently: ``build_and_run`` and
  ``python_script``. See the main docstring in
  ``testsuite/testsuite_support/*.py`` to learn more about them.

Here's an example for a minimal ``test.yaml``:

.. code-block:: yaml

    description:
        Tests foo's special bar feature in the baz error case.

    driver: screw_driver

Also create a ``test.out`` file that contains the expected output for this
testcase to pass. That's all for the mandatory part. Now, depending on your
testcase and your driver, you will also add various files next to this
``test.yaml``.


Expected failures
-----------------

Sometimes, bugs stay longer than we would like them to, and we want them not to
be flagged as "new regressions" because it is permanent noise. Expected
failures are a way for specific testcase to be flagged as "it fails, but we
know about it, here's the ticket that will fix this: XXX-XXXX".

Doing so is easy: in the ``test.yaml`` file, add a ``expect_failure`` key with
a message that describes the situation. For instance:

.. code-block:: yaml

    expect_failure:
        foobarbaz is a nasty error case, deep implication in our
        implementation, will be fixed some day under XXX-XXX.

Next time you run this testcase, either it fails, in which case it will be flagged as
XFAIL (eXpected FAILure), or it succeeds, then it will be flagged as UOK
(Unexpected OK).
