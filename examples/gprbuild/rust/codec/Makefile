all: default.cgpr
	gprbuild -p -Pcodec $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Rust

clean: default.cgpr
	gprclean --remove-cargo-build-dir -Pcodec
	$(RM) default.cgpr rust/Cargo.lock

run: all
	./rust/target/*/release/encode
	./rust/target/*/release/decode

.PHONY: all clean run
