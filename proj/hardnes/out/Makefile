.PHONY: flash
flash:
	openFPGALoader -b nexys_a7_100 _out/hardnes_nexys.bit

.PHONY: build
build:
	./build.sh

.PHONY: uart
uart:
	mkdir -p _term
	cd _term && tio -b 115200 /dev/ttyUSB1 --parity even