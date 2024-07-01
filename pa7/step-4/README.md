## Adding information to Op Codes

A benefit of using op codes is that we can fold information into the operation itself.

Specifically, our left and right bracket opcodes can store the information about where
that specific op jumps to. For example, LBrack(30) is a left bracket, that if taken, 
jumps the program counter to position 30.

This removes the need to store auxilary datastructures like our bmap, and it opens
the path towards future optimizations.