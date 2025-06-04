riscv64-linux-gnu-as -march=rv64i -o $1.o $1.s
riscv64-linux-gnu-ld -o $1 $1.o
qemu-riscv64 ./$1
