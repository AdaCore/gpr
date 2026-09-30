all: default.cgpr
	gprbuild -p -Phello_from_ada $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config Rust

clean: default.cgpr
	gprclean --remove-cargo-build-dir -r -Phello_from_ada
	$(RM) default.cgpr rust/Cargo.lock

run: all
	./hello_from_ada

.PHONY: all clean run
