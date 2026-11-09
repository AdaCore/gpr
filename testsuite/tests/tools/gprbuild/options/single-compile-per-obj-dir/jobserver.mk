.PHONY: all
all:
	$(info MAKEFLAGS=$(MAKEFLAGS))
	+"$(GPRBUILD)" -P tree/jobserver.gpr -p -q -j4 -U -f --single-compile-per-obj-dir
