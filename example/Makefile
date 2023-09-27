all: hello.bin

.PHONY: ../target/debug/ccpu-cc

hello.bin: hello.o ../ccpu-runtime/runtime.o ../ccpu-runtime/divide32.o ../ccpu-runtime/memcpy.o ../ccpu-runtime/rom_startup.o ../../ccpu-libc/libc.a
	../../ccpu/tools/link.py -o $@ --slim --layout rom.yaml -m $(@:.bin=.map) $^

hello.app: hello.o ../ccpu-runtime/runtime.o ../ccpu-runtime/divide32.o ../ccpu-runtime/memcpy.o ../ccpu-runtime/app_startup.o  ../../ccpu-libc/libc.a
	../../ccpu/tools/link.py -o $@ --slim --layout app.yaml -m $(@:.app=.map) $^

%.o: %.c ../target/debug/ccpu-cc
	cargo run -- --verbose -I ../../ccpu-libc/include -o $(@:.o=.s) $< > $(@:.o=.out)
	../../ccpu/tools/asm.py -o $@ $(@:.o=.s)

%.o: %.asm
	../../ccpu/tools/asm.py -o $@ $<

../target/debug/ccpu-cc:
	cd .. && cargo build
