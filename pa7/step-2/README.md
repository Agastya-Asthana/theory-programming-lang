## Speeding up the interpreter with jump caching

A classic way of optimising any programming language implementation is to process the program before executing it with the aim of moving work from during-program-execution to before-execution-processing. In the case of our simple BF interpreter from above, an obvious problem is the way it treats BF's square brackets [...]. When the interpreter encounters either the opening or closing square bracket, it has to skip forwards/backwards for the matching square bracket, bearing in mind that brackets can be nested. Fortunately, since BF programs can't modify themselves, we always jump forwards/backwards to the same location. Thus we can precalculate the jumps, store them in a cache, and turn the dynamic searches (using a while loop) into a simple lookup in a vector.


## Algorithm pseudo-code

```
create a stack
create a hashmap
for position and token in program
    if the token is an open bracket, add the position to the stack
    else if the token is a close bracket
        update the map to point the top element of the stack to current position
        update the map to point the current position to the top element of the stack
        pop the top element of the stack
```

## Updating our `[` and `]` instructions

Now that we have a mapping for where each bracket will jump to, our command handler is much simpler.
We can look at where we are in the current program, and look up where that leads to in the hashmap.
