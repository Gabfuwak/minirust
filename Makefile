AS = riscv64-linux-gnu-as
LD = riscv64-linux-gnu-ld -m elf32lriscv
QEMU = qemu-riscv32

RISCV_TESTS = $(wildcard backend_tests/*.rs)
RISCV_NAMES = $(basename $(notdir $(RISCV_TESTS)))

all:
	dune build

# Usage: make riscv FILE=tests/01
riscv:
	@echo "=== Building $(FILE).rs → RISC-V ==="
	RISCV=1 dune exec minirust/minirust.exe $(FILE).rs > $(FILE).s
	$(AS) -march=rv32im -o $(FILE).o $(FILE).s
	$(LD) -o $(FILE) $(FILE).o
	@echo "✓ Done: $(FILE)"
	@echo "✓ Run: $(QEMU) ./$(FILE)"

run:
	@$(MAKE) riscv FILE=$(FILE)
	$(QEMU) ./$(FILE)

test-riscv:
	@echo "=== Running RISC-V backend tests ==="
	@passed=0; total=0; \
	for test in $(RISCV_NAMES); do \
		total=$$((total + 1)); \
		echo -n "Testing $$test... "; \
		if [ -f backend_tests/$$test.expected ]; then \
			expected=$$(cat backend_tests/$$test.expected); \
			$(MAKE) riscv FILE=backend_tests/$$test > /dev/null 2>&1; \
			actual=$$($(QEMU) ./backend_tests/$$test 2>/dev/null); \
			if [ "$$actual" = "$$expected" ]; then \
				echo "✓ PASS ($$expected)"; \
				passed=$$((passed + 1)); \
			else \
				echo "✗ FAIL (expected '$$expected', got '$$actual')"; \
			fi; \
		else \
			$(MAKE) riscv FILE=backend_tests/$$test > /dev/null 2>&1; \
			if [ $$? -eq 0 ]; then \
				echo "✓ PASS (compile only)"; \
				passed=$$((passed + 1)); \
			else \
				echo "✗ FAIL (compile error)"; \
			fi; \
		fi; \
	done; \
	echo "=== Results: $$passed/$$total tests passed ==="

clean:
	dune clean
	rm -f backend_tests/*.s backend_tests/*.o
	cd backend_tests && rm -f $$(ls | grep -v '\.')

.PHONY: all riscv run clean
