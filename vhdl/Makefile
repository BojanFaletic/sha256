all:
	ghdl -a sha_types.vhd
	ghdl -a k_memory.vhd
	ghdl -a sha256.vhd
	ghdl -a sha256_tb.vhd

	ghdl -e k_memory
	ghdl -e sha256
	ghdl -e sha256_tb

	ghdl -r sha256_tb

clean:
	ghdl --clean
	rm -f work*