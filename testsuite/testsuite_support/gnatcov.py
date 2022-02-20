import glob
import logging
import os.path
import shutil

from e3.os.process import Run, quote_arg


class GNATcov(object):
    """
    Helper to compute code coverage with GNATcoverage using the testsuite.

    After initialization, the workflow for this helper is:

    * call the `report()` method to generate coverage reports.
    """

    def __init__(self, testsuite):
        """
        :param Testsuite testsuite: Testsuite instance to work with.
        """
        self.level = testsuite.env.options.level
        self.sid_dir = testsuite.env.options.gnatcov

        self.temp_dir = os.path.join(testsuite.env.working_dir, 'gnatcov')
        self.output_dir = testsuite.output_dir

        self.ensure_clean_dir(self.temp_dir)
        os.mkdir(self.traces_dir)

    @property
    def traces_dir(self):
        """ The gnatcov's trace file destination folder """
        return os.path.join(self.temp_dir, 'traces')

    @staticmethod
    def ensure_clean_dir(dirname):
        """
        If it exists, remove the ``dirname`` directory tree and create an empty
        directory instead.
        """
        if os.path.exists(dirname):
            shutil.rmtree(dirname)
        os.mkdir(dirname)

    @staticmethod
    def checked_run(argv):
        """
        Run a process with the given arguments. Log its output and raise an
        error if it fails.
        """
        p = Run(argv)
        if p.status != 0:
            logging.error('Command failed: %s',
                          ' '.join(quote_arg(arg) for arg in argv))
            logging.error('Output:\n' + p.out)
            raise RuntimeError

    def report(self, formats=['dhtml', 'xcov']):
        """Generate coverage reports for all given output formats."""

        # Get the list of all trace files
        traces_list = os.path.join(self.temp_dir, 'traces.txt')
        with open(traces_list, 'w') as f:
            for t in glob.glob(os.path.join(self.traces_dir, '*.srctrace')):
                f.write(t + '\n')

        # Load trace files only once, produce a checkpoint for them
        logging.info('Consolidating coverage results')
        ckpt_file = os.path.join(self.temp_dir, 'report.ckpt')
        self.checked_run(['gnatcov', 'coverage', '--level', self.covlevel,
                          '-P', 'gpr2-tools',
                          '-XGPR2_BUILD=gnatcov',
                          '--externally-built-projects',
                          '--save-checkpoint', ckpt_file,
                          '@' + traces_list])

        # Now, generate all requested reports from this checkpoint
        logging.info('Generating coverage reports ({})'
                     .format(', '.join(sorted(formats))))
        for fmt in formats:
            report_dir = os.path.join(self.output_dir, 'coverage-' + fmt)
            self.ensure_clean_dir(report_dir)
            self.checked_run([
                'gnatcov', 'coverage',
                '--annotate', fmt,
                '--level', self.covlevel,
                '--output-dir', report_dir,
                '-P', 'gpr2-tools',
                '--externally-built-projects',
                '-XGPR2_BUILD=gnatcov',
                '--checkpoint', ckpt_file])

    @property
    def covlevel(self):
        """ The gnatcov's --level value used during this testsuite's run """
        return self.level
