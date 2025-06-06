# Project

This project contains the implementation of a front-end and two(ish) back-ends for the language Minirust, a toy subset of Rust. 

This is a university project which goal was to build a borrowchecker for Minirust. I added my own extension, namely a C back-end that I was not truly satisfied with and that gave life to a RISC-V back-end. 

All the front-end except for [uninitialized_places.ml](https://github.com/Gabfuwak/minirust/blob/master/lib/uninitialized_places.ml "uninitialized_places.ml") and [borrowck.ml](https://github.com/Gabfuwak/minirust/blob/master/lib/borrowck.ml "borrowck.ml") were given as base code.

This project comes with 94 tests for the front-end and 37 for the RISC-V back-end. The C back-end should work, but it is mostly untested and is there more as a proof of concept and to show its interesting implementation details. 
# Dependencies

All front-end dependencies are available in [minirust.opam](https://github.com/Gabfuwak/minirust/blob/master/minirust.opam "minirust.opam"), while the test suite for the RISC-V backend uses `qemu-riscv32` and `riscv64-linux-gnu-(as|ld)` as assember and linker. Don't hesitate to change the variables in the [Makefile](https://github.com/Gabfuwak/minirust/blob/master/Makefile "Makefile") to suit your needs if the default setup does not work on your machine (tested only on Arch Linux with the previously mentioned emulator, assembler and linker).

# Usage

**Generate Minimir:**
```sh
MINIMIR=1 dune exec minirust/minirust.exe [path_to_minirust_file]
```

**Generate C:**
```sh
# with no sysenv tag
dune exec minirust/minirust.exe [path_to_minirust_file] [path_to_output]
```

**Generate RISC-V assembly**
_Note: The assembly will be written as a .s file in the same folder and with the same name as the .rs input file if you use make_
```sh
make riscv FILE=[path_to_minirust !WITHOUT FILE EXTENSION!]
# or
RISCV=1 dune exec minirust/minirust.exe [path_to_minirust_file] [path_to_output]
```

**Run a file using the RISC-V backend**
```sh
make run FILE=[path_to_minirust !WITHOUT FILE EXTENSION!]
```

**Run all front-end tests**
```sh
dune runtest
```

**Run back-end tests**
```sh
make test-riscv
```

# Implementation

## Borrowchecker

The only notable implementation details in the borrowchecker are the recursive functions to unify type lifetimes and collect lifetime borrows. The borrowchecker implementation consisted of mostly reading a lot of code to understand what I actually needed to do.

## Extension: the back-end(s)
### C back-end

The C back-end translates the minimir bodies and struct declarations into valid C code.

The C generation is processed in this order:

***1. Creation of the struct typedefs***

While I tried to make it as simple as possible for myself, it is not possible to create struct prototypes in C. This step therefore involves the creation of a dependency graph for the structs, followed by a topological sort using Khan's algorithm to generate a correct order of generation for the structs.

An important detail is that a topological sort requires a DAG as the dependency graph. This is not the case in Minirust, nor in C. This for example is completely legal to write:
```rust
struct X<'a> {
    a: &'a Y<'a>
}

struct Y<'a> {
    val: X<'a>
}
```

We navigate around this problem by not adding a dependency when a struct element is a borrow.
```ocaml
(* Function that returns the list of struct dependencies for any type *)
let extract_type_deps typ =
  match typ with
  | Tstruct (name, _) -> [name]
  | Tborrow _ -> []
  | Tunit | Ti32 | Tbool -> []
```

This edge case is not quite finished at the moment, but it should write a forward declared struct:
```c
typedef struct Y Y;

typedef struct {
	const Y* a;
} X;

struct Y {
	X val;
};
```


***2. Creation of the function prototypes to avoid dependency graphs***

The second stage of the C generation process is creating the function prototypes. The only notable part is that while creating the prototypes, we store the mir bodies in a list to then use them in step 3 without calling emit_mir again.

***3. Creation of the function implementations***
We first call the code from the prototype generation to create the function's first line, then we generate the locales from minimir, and we finish by translating the instructions.

The basic principle is that for each minimir instruction, we create a label and translate this instruction to C. At the end of the instruction we "goto" to the next instruction. This makes the C very assembly-like without idiomatic control flow and expressions. For example, a simple if condition gives this result for the :

```c
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    int32_t age;
} Person;

int32_t main(Person p);


int32_t main(Person p){
    int32_t _localvar2;
    bool _localvar1;
    int32_t _localvar3;
    int32_t _localvar4;
    int32_t _ret;
L0:
    _localvar3 = p.age;
    goto L1;
L1:
    _localvar4 = 100;
    goto L2;
L2:
    _localvar1 = (_localvar3 < _localvar4);
    goto L3;
L3:
L4:
L5:
    if (_localvar1) {
        goto L6;
    } else {
        goto L7;
    }
L6:
    _ret = p.age;
    goto L10;
L7:
    _localvar2 = 1;
    goto L8;
L8:
    _ret = -(_localvar2);
    goto L9;
L9:
L10:
L11:
    return _ret;
}
```

_Note: Right before starting the RISC-V extension, I considered doing a control flow graph analysis of the minimir code, before realising it was meaningless as I had an AST that I could get all the idiomatic structures from._

While this approach produces correct and compilable C code, the output lacks the idiomatic control flow structures that would make it readable and maintainable. This limitation motivated the development of a RISC-V backend.

### RISC-V back-end

After my disappointing C experience, I decided that since the C code was so assembly-like, it should not be too hard to make an assembly back-end and it would make a good excuse to learn RISC-V (famous last words).

#### Stack and memory management
The memory management is easily the hardest part of dealing with assembly, this section is the most complex of the whole project.
##### Stackframe and offsets
In this implementation, the stack is static. We calculate the total amount of resources statically allocated at the beginning of each function, and allocate space on the stack. At the end of the function, we free this space. We allocate at least 64 bytes for each function and add a 16 bytes margin for ra and just general precaution.
```ocaml
  let (curr_offset, offset_table) = compute_all_offsets prog fundef mir_body in
  let frame_size = max 64 (curr_offset + 16) in 

  (* Emit function label*)
  Printf.fprintf oc "%s:\n" fundef.fname.id;

  (* Allocate stack *)
  Printf.fprintf oc "%saddi sp, sp, -%d\n" indent frame_size;
```
Since the stack is static, we do not need to bother with the alignment.

We then allocate ra at the end of the stack frame. 
```ocaml
Printf.fprintf oc "%ssw ra, %d(sp)\n" indent (frame_size - 4);
```

We store all the local variables and arguments in the offset hash table, knowing that the first 8 arguments are stored in register a0 to a7, the next are on the stack (not fully implemented, the back-end sadly only handles up to 8 arguments for a function right now). We can recover all these memory locations when needed.
##### Structs
A struct is represented as a pointer to its first element. It is **always** on the stack. Technicaly a 4 byte struct would fit in a register, but it is not a supported optimisation. A struct is a pointer to its first element in the stack. We can get the offset of the different fields using `field_offset prog struct_name field_name`. The struct is packed as tightly as it can be, and we again do not need to bother for padding since Minirust does not support arrays.
##### Locations
To find a place in the assembly memory, we use the `riscv_location` type.
```ocaml
type riscv_location = 
  | ComputedStack of int           (* offset in relation to sp *)
  | ComputedReg of string * int    (* register + offset *)
  | Indirect of riscv_location     (* deref ptr *)
  | FieldAccess of string * int    (* struct field access *)
```
Data can be located in the stack, in a register, indirectly with an address stored in some location, or in a struct (on the stack).

A ComputedReg with an offset of 0 is just a register value itself, while a FieldAccess with an offset of 0 is the first element at the address contained in the register (represented by a string here).

To access these fields, we use different fucntions:
- `location_of_place` : place -> riscv_location 
- `riscv_of_location` : riscv_location -> string (which generates simple code, like "a0" or "-4(sp)")

For more difficult cases, such as reborrows or accessing structs inside of registers, we need to use temporary registers, so riscv_of_location is not suitable. In this case, we use helper functions depending on our use case that will treat all of the edge cases. The simplest of which is meant to calculate the address of a location on the stack, and put it inside of t0.

```ocaml
let emit_address_calculation oc borrowed_loc =
  match borrowed_loc with
  | ComputedStack offset ->
      Printf.fprintf oc "%saddi t0, sp, %d\n" indent offset
  | ComputedReg (reg, offset) | FieldAccess (reg, offset) ->
      Printf.fprintf oc "%saddi t0, %s, %d\n" indent reg offset
  | Indirect pointer_loc -> emit_load_to_register oc pointer_loc "t0"
```

The other functions are:
- `emit_place_to_register`,  which generates code to take a value from its minimir place representation and put it in a register,handling all edge cases
- `emit_assignment`, which generated code to assign a riscv_location's value to an other riscv_location
- `emit_load_to_register`, which loads the value from a location to a register

They are all recursive (except for `emit_address_calculation`) to handle the potentially chained indirections.

#### Code generation
The RISC-V generation is processed in two steps:

***1. Generation of the boilerplate***
We first generate the `_start` function, which is hard coded to call the main function of the program. If after the mir_bodies processing there is still no main, we generate an empty one.

We then add a print_int function. This is to make the debug suite work without having to output in the exit codes and potentially miss real errors. This code is adapted from [a StackOverflow question](https://stackoverflow.com/questions/66941555/how-to-output-an-integer-to-screen-risc-v-assembly)

***2. Generation of the functions***
We then generate all functions, one by one.

They have their name as label, and in_function label are in the form `[fname]_L[minimir_instr_idx]`. 

The `emit_function` function is quite long, especially the assignment instruction and I invite you to read it. I tried to make is as understandable as possible.

An important limitation here is that the arguments are stored on the a[0-7] registers. I did not have the time to implement the function arguments overflowing on the stack, so more than 8 arguments in a function is not supported.

#### Testing suite

Having encountered a lot of struggle in the RISC-V assembly, I decided to add unit tests. These tests work with a make function running all .rs files from the backend_tests folder. The functions are hard coded to print the return value of the main function to the console. We compare that value to .expected files. If no such file exist, we just check if this compiles. 

There are a total of 37 tests, all successfully passing.

# Struggles

This project was overall a lot of fun, but it did not come without struggles. The biggest difficlty was, in the initial stages of development, understanding the codebase that was given as a starting point. It is very complex and took many hours of reading code, trial and error to get into.

In more details, the lifetime unification was by far the most challenging part of the mandatory tasks, with a lot of sneaky bugs and tough to understand concepts, especially around type unification and reborrows. I actually did most of this task right after the first one, skipping over task 2 by mistake. After this experience the rest seemed easier.

The backends were where most of my struggles happened. For the C back-end, I decided to generate generate the function prototypes first, avoiding a lot of dependency graph headache, but a topological sort was necessary in the end for struct definitions, which was a bit challenging to get working.

In the RISC-V backend, the biggest diffculty was by far memory managment. I'm not particularly happy about my solutions for riscv location handling, but they worked. The borrows and structs of values stored in registers took a very long time to get right and almost compromised the completion of the extension. About 75% of my last two days of work (~15 commits) were struct/borrow related and the final upgrade to the test suit is entirely meant to debug these.
