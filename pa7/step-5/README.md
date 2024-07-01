## Repeated Op Codes

One obvious inefficiency in BF programs is repeated sequences of the same instruction. For example to increment a cell by 3, I have to use three add instructions "+++". We can optimise these repeated opcodes by adding a simple count to an instruction, replacing Add with Add(3) for this simple example. Let's start by doing this just for the Add opcode.

This is called a [look ahead parser](https://en.wikipedia.org/wiki/LALR_parser). It is often helpful to know what the next token or tokens are while parsing a program.

## Algorithm pseudo-code

```
let bytes be the provided program

for i in length of bytes
    if '+' then
        let iter be all the bytes from current position to end of bytes
        let count be the number of '+' characters that start that iter
        add to the program an Op::Add(count)
        set i to be (i + count - 1)
```

Hint: use the same construct of a tuple as in the bracket matching step.