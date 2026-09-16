all: default.cgpr
	gprbuild -p -Pmain $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config Rust

clean: default.cgpr
	gprclean --remove-cargo-build-dir -r -Pmain
	$(RM) default.cgpr rust/Cargo.lock

run: all
	./rust/target/*/release/main_from_rust

.PHONY: all clean run
