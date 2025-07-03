import glob
import logging
import os.path
import re
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
    def gpr2_project_dir(self):
        "Find the gpr2 project directory"
        paths = os.environ.get("GPR_PROJECT_PATH", "").split (os.pathsep)
        for p in paths:
            if os.path.exists(os.path.join(p, "gpr2.gpr")):
                return p
        return None

    @property
    def traces_dir(self):
        "The gnatcov's trace file destination folder"
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

    def checked_run(self, argv):
        """
        Run a process with the given arguments. Log its output and raise an
        error if it fails.
        """
        p = Run(argv)
        if p.status != 0:
            logging.error('Command failed: %s',
                          ' '.join(quote_arg(arg) for arg in argv))
            logging.error('Output:\n' + p.out)

            # Look for the name of a source trace if the output, and if we find
            # one, display some context so that we know how that source trace
            # was created.
            srctrace_re = re.compile("[0-9]+\\.srctrace")
            for filename in srctrace_re.findall(p.out):
                logging.error("Found a mention of a source trace: " + filename)
                context_filename = os.path.join(
                    self.traces_dir, filename + "-context.txt"
                )
                try:
                    f = open(context_filename)
                except IOError:
                    logging.error("No context for that source trace")
                else:
                    with f:
                        logging.error(f.read())

            raise RuntimeError

    def report(self, formats=['dhtml', 'xml', 'cobertura']):
        """Generate coverage reports for all given output formats."""

        gnatcov_base_args = [
            'gnatcov',
            'coverage',
            '--level',
            self.covlevel,
            '-P',
            'gpr2',
            '-XGPR2_BUILD=gnatcov',
            '--externally-built-projects',
            '--no-subprojects',
        ]

        # Get the list of all trace files
        traces_list = os.path.join(self.temp_dir, 'traces.txt')
        with open(traces_list, 'w') as f:
            for t in glob.glob(os.path.join(self.traces_dir, '*.srctrace')):
                f.write(t + '\n')

        # Load trace files only once, produce a checkpoint for them
        logging.info('Consolidating coverage results')
        ckpt_file = os.path.join(self.temp_dir, 'report.ckpt')
        self.checked_run([
            *gnatcov_base_args,
            '--save-checkpoint',
            ckpt_file,
            '@' + traces_list,
        ])

        # Now, generate all requested reports from this checkpoint
        logging.info('Generating coverage reports ({})'
                     .format(', '.join(sorted(formats))))
        for fmt in formats:
            report_dir = os.path.join(self.output_dir, 'coverage-' + fmt)
            self.ensure_clean_dir(report_dir)
            path_opt = []
            if fmt == 'cobertura':
                # we need paths relative to the project root in this case
                gpr2_path = self.gpr2_project_dir
                if gpr2_path is not None:
                    path_opt = ['--source-root=' + gpr2_path]
            self.checked_run([
                *gnatcov_base_args,
                '--annotate',
                fmt,
                '--output-dir',
                report_dir,
                '--checkpoint',
                ckpt_file,
                *path_opt
            ])

    @property
    def covlevel(self):
        """ The gnatcov's --level value used during this testsuite's run """
        return self.level
