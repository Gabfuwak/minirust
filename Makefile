AS = riscv64-linux-gnu-as
LD = riscv64-linux-gnu-ld -m elf32lriscv
QEMU = qemu-riscv32

all:
	dune build

# Usage: make riscv FILE=tests/01
riscv:
	@echo "=== Building $(FILE).rs → RISC-V ==="
	RISCV=1 dune exec minirust/minirust.exe $(FILE).rs > $(FILE).s
	$(AS) -march=rv32i -o $(FILE).o $(FILE).s
	$(LD) -o $(FILE) $(FILE).o
	@echo "✓ Done: $(FILE)"
	@echo "✓ Run: $(QEMU) ./$(FILE)"

run:
	@$(MAKE) riscv FILE=$(FILE)
	$(QEMU) ./$(FILE)

clean:
	dune clean
	rm -f tests/*.s tests/*.o tests/[0-9][0-9]

.PHONY: all riscv run clean
