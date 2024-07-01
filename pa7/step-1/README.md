## Writing an interpreter for BrainF*ck

In `interp.js`, you have been provided with a skeleton code for handling a program written in `bf`. Read more about the language on [wikipedia](https://en.wikipedia.org/wiki/Brainfuck).

The basics of the language are as follows:

1. The runtime has an infinite tape, representing the memory of the program (we limit this to 10000).
2. The runtime maintains a program counter (`pc`) which tracks the position of the executing command in the program.
3. The runtime maintins a data pointer, or cell counter (`cc`), which points to a specific location on the tape.
3. There are 8 total commands, each only one character.

```
+ : increment value in memory at the `cc` by 1.
- : decrement value in memory at the `cc` by 1.
> : shift `cc` to the right by 1.
< : shift `cc` to the left by 1.
. : output (print) the byte at `cc`.
, : read input to the byte at `cc`.
[ : If the byte at `cc` is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
] : If the byte at `cc` is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.
```


## Assignment

Write the interpreter for the `bf` language. You'll complete the missing sections of `interp.js` to handle each of the commands listed above.

## Testing

Provided are a few different `.bf` files. After building, you can run with `node test.js` from the assignment's root directory. Change the `test.js` file as you progress to test your advanced implementations.
