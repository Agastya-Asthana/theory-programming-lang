## Adding Op Codes and changing the representation

Our first two BF interpreters both load the input program in and execute it directly. In contrast, mainstream language implementations such as Python read a program in and translate it to a different representation. We lack common terminology for what to call this different representation, so we will call it a sequence of opcodes.

We now modify our interpreter by adding an enum, with one opcode per BF instruction as a starting point.

This feels unnecessary for now, but we will start using these Op codes to great effect in the next few steps.