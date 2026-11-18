The ALI fixtures in `tree/obj` were generated with GNAT Pro 28.0w 20261115 (GCC
16.2.1 20261116), x86_64-linux.
The source files and project used to generate them are retained in `tree`.

Regenerate from this test directory with:

```sh
gprbuild -p -q -c -U -Ptree/prj.gpr
```

Only these compiler outputs are fixtures: `pkg.ali`, `main.ali`,
`generic_pkg.ali`, `preelab_pkg.ali`, `pure_pkg.ali`.
Object files and other build products are not needed by the test.
