# Program Verification, PA8

Your task in the programming assignment is to create a verifier for the first three programming assignments.

Program verification is a powerful field which we will only briefly cover in lecture. In its simplist form, we use unit tests to approximate property verification. For a given program `p` and an input `x`, if `p(x)` produces an expected output then the test case passes and we have verified our program on a single input.

Given the often large input space of possible values to our programs, we typically look to solve for classes, or sets, of inputs which we can write test cases for to verify many possible scenarios at once. Consider the simple example, `p=abs` (the absolute value function). When deciding what test cases should be written, it would be wise to consider each boundary where functionality changes. `p(3)` and `p(30)` will likely behave very similarly despite their distance, however `p(1)` and `p(-1)` will be quite different.

An automatic approach to take towards verification is to look at the symbolic meaning of the function. For example, if you are given a program:

```javascript
// p=abs
let p = (x) => {
    if (x < 0) {
        return -1 * x;
    }
    return x;
}
```

We can perform static analysis to see that for an input `x` we will receive either `x` or `-x` as the return value. We quantify that `p(x < 0) => -x` and `p(x >= 0) => x`, which is precisely the property that `abs` should have. The process of finding these properties falls under the field of [Symbolic Execution](https://en.wikipedia.org/wiki/Symbolic_execution). In this example, the functionality is obvious and contrived. Let's consider a slightly more logically complicated example:

```javascript
let p = (a, b, c) => {
    let x=0, y=0, z=0;

    if(a) {
        x = -2; 
    }
    if (b < 5) {
        if (!a && c) { y = 1; }
        z = 2;
    }
    assert(x + y + z != 3);
}
```

Which set of inputs cases cause the above snippet to fail? A tester could either try every combination of a, b, and c, or we can take an intelligent approach to explore the boundaries of `a = true | false`, `b = 5 | 4`, `c = true | false`, cutting the search space to only 8 possible categories of output. Again, the process of boundary detection can seem quite straightforward to a human, but to automatically detect them is a non-trivial task. For more on this, see [Lecture Notes on Symbolic Execution](https://www.cs.cmu.edu/~aldrich/courses/17-355-18sp/notes/notes14-symbolic-execution.pdf)

## Enough preamble...

These notes got away from me as they often do. The assignment itself will be composed of 2 sections. First, you will write unit tests for the first three programming assignments. Second, you will detect simple boundary conditions and provide a set of values that pass an unknown piece of code and a set of values that fail the code.

## Part 1: Unit testing

Problem formulation: for each function that you implemented in pa1, pa2, and pa3, you will write complete test cases which effectively differentiate a passing function and a non-passing function.

Fill in the `pa{x}_unit_tests.js` with your tests. I have added stubs for the functions that I will call with various implementations of each function. Your implementation will return `true` if the function being tested passes your test cases and `false` if it does not. For example:

```javascript
let test_fold_left = (fold_left) => {
    // TODO: evaluate this implementation of fold_left
    return false;
}
```

I recommend adding a sequence of tests to each of these to keep your testing maintainable.

```javascript
let test_fold_left = (fold_left) => {
    let answer = my_test_fold_left_1(fold_left);
    answer = answer && my_test_fold_left_2(fold_left);
    answer = answer && my_test_fold_left_3(fold_left);
    // ...
    return answer;
}
```

During testing, I will pass many different styles and implementations. You must not only ensure correctness but also that the code is written to the spec of the assignment, i.e. in a functional style. *Iterative, while/for, implementations should not pass your tests.*

## Part 2: Boundary Detection

Problem forumulation: given a faulty JavaScript program, create a sampling function which outputs a set of input variables which do not raise an exception in the program, and a second set of input variables which do raise an exception in the program (see snippet 2 for an example program).

First, consider how you would solve this problem if all that you had were the function itself, i.e.

```javascript
let test_unknown_func = (unknown_func) => {
    // TODO: Find a boundary in the function
    // ...
    return answer;
}
```

Take a second and think this one through. What would you do?

By the time you hit runtime, this problem becomes very tricky, you could potentially use a runtime inspector to look at the bytecode/partially compiled instructions for the `unknown_func` but it is unclear what you will find or if you even have access to that information.


Now consider how you would solve this if you were presented with the filename and function name in that file to test, i.e. (notice parameters have changed)

```javascript
let test_unknown_func = (js_filename, func_name) => {
    // TODO: Find a boundary in the function
    // ...
    return answer;
}
```

Take a second and think this one through. What would you do?

We are now at the "pre-compile" time. This file is sitting statically on your system and you have no information about it other than the text that lives within it. Will you use a regex to look for the function defintion? What would you do if it is arrow notation (`(args) => { ... }`, or `(args) => value`, or `arg => value`) instead of traditional function notation (`function(args) { ... }`).

This is still a tricky problem to solve, albiet an easier one than the runtime option.


Finally, consider a middle ground, what if you are provided with the abstract syntax tree of the function directly, i.e.

```javascript
let test_unknown_func = (func_ast) => {
    // TODO: Find a boundary in the function
    // ...
    return answer;
}
```

The AST has been parsed from the target file and already knows the key idea of the function. It has a `name` node, a `parameters` node, and a `body` node. The `parameters` node will be a list of identifiers (variable names) which might have default values or type information. The `body` node is itself a sequence of nodes, such as `assignment` nodes, `call` nodes, etc.

Now we can start looking for patterns and performing logical operations on the AST. Perfect!