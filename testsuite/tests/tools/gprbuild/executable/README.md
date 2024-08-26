This test has been divided into three parts, allowing each part to be run in
parallel. This approach is particularly useful for Valgrind runs, which could
take up to 30 minutes in the worst-case scenario if the test were kept as
a single unit.