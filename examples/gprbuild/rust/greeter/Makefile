all: default.cgpr
	gprbuild -p -Pgreeter $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Rust

clean: default.cgpr
	gprclean --remove-cargo-build-dir -Pgreeter
	$(RM) default.cgpr rust/Cargo.lock

run: all
	@echo "greeter is a library, there is nothing to run:"
	@ls rust/target/*/release/libgreeter.*

.PHONY: all clean run
