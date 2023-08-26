all: hello.bin

.PHONY: ../target/debug/ccpu-cc

hello.bin: hello.o ../ccpu-runtime/runtime.o ../ccpu-runtime/rom_startup.o
	../../ccpu/tools/link.py -o $@ --slim --layout rom.yaml -m $(@:.bin=.map) $^

hello.app: hello.o ../ccpu-runtime/runtime.o ../ccpu-runtime/app_startup.o
	../../ccpu/tools/link.py -o $@ --slim --layout app.yaml -m $(@:.app=.map) $^

%.o: %.c ../target/debug/ccpu-cc
	../target/debug/ccpu-cc --verbose -o $(@:.o=.s) $<
	../../ccpu/tools/asm.py -o $@ $(@:.o=.s)

%.o: %.asm
	../../ccpu/tools/asm.py -o $@ $<

../target/debug/ccpu-cc:
	cd .. && cargo build
