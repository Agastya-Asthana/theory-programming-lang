## Multicode Optimization

At this point we've arguably done the easy stuff and now we have to think a bit more deeply about what parts of a common BF program might slow down. One common BF idiom is a while loop that resets a cell to zero: its running time is proportional to the current value of the cell (i.e. the higher the current value in the cell, the longer it takes to reset to zero!). In terms of our opcodes this results in a sequence [LBrack, Sub(1), RBrack].
There is no way that we can optimise this opcode sequence in terms of the opcodes we currently have. But, fortunately, adding opcodes is easy, and the BF program never need know we've done it! Let's add a new opcode Zero which simply resets a cell to zero directly, and replace all [LBrack, Sub(1), RBrack] sequences with a single Zero opcode.

## Algorithm PseudoCode

```
let i = 0
while i < program length 
    if progam[i..i+3] == [Ops.LBrack, Ops.Sub, Ops.RBrack]
        replace program[i..i+3] with Ops.Zero
        i = i + 3
    else
        i = i + 1
```

## Significance 

At this point, we have come from a very simple interpret to a rather complicated compiled language.
`interp7` has all the hallmark components of a true compiler: a parsing/compiling stage where we convert
the input program into tokens, each of which can be a complex unit; an optimization phase where we iterate
through the program in search of idiomatic or language level improvements; an evaluation phase where we
get the program results. Many compilers will separate the evaluation phase to be outputing an executable for
later execution, but the idea remains the same.