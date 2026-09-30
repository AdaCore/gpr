all: default.cgpr
	gprbuild -p -Phello_from_rust $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Rust

clean: default.cgpr
	gprclean --remove-cargo-build-dir -Phello_from_rust
	$(RM) default.cgpr rust/Cargo.lock

run: all
	./rust/target/*/release/hello_from_rust

.PHONY: all clean run
