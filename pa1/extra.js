const { List } = require("immutable");
const { fold_left, filter, map } = require("./quick");

/*
  The following problems will give you a chance to practice higher order
  functions. To solve these problems, you can only use map, filter, fold_left
  (see quick.js), partition, or a combination of these. You should not use for-
  or while-loops.  For each problem, a simple assertion is provided to test
  your implementation.  You may wish to add additional tests, though.
*/

let ls = List([1, 2, 3, 4, 5]);

/* QUESTION 1: Average of squares */

/*
  First, implement a function that returns the size of a list using fold_left.
*/

const ls_size = (ls) => fold_left((base, el) => base + 1 , 0, ls)


/*
  Second, implement a function that computes the sum-of-squares of a list of
  integers using fold_left.
  Example: sum_sqrs([2,3]) => 2*2 + 3*3 = 13.
*/

const sum_sqrs = (ls) => fold_left((base, el) => base + el * el, 0, ls)


/*
  Finally, use both functions to implement the average of squares function.
*/

const avg_sqrs = (ls) => sum_sqrs(ls) / ls_size(ls)


/* QUESTION 2: Min/Max and Evens/Odds */

/*
  Implement min and max functions using fold_left

  An empty list has no minimum or maximum value, therefore, return undefined.
*/

const ls_min = (ls) => fold_left((acc, el) => {
  if (acc === undefined || el < acc){
    return el
  }
  return acc
}, undefined, ls)
const ls_max = (ls) => fold_left((acc, el) => {
  if (acc === undefined || el > acc){
    return el
  }
  return acc
}, undefined, ls)


/*
  Implement two functions that return evens and odds from a list of integers
  using filter
*/

const ls_evens = (ls) => filter((el) => el % 2 === 0, ls)
const ls_odds = (ls) => filter((el) => el % 2 !== 0, ls)

/*
  Using function composition, implement a function that returns the maximum
  number in the even subset of a given list and another function that returns
  the minimum number in the evens. Recall the definition of compose below:
*/
function compose(f, g) {
  return (x) => {
    return f(g(x));
  };
}

const max_even = (ls) => ls_max(ls_evens(ls))
const min_even = (ls) => ls_min(ls_evens(ls))


/* QUESTION 3: The reverse function and Palindromes */

/*
  Implement reverse using recursion
*/

const reverse = (ls) => {
  if (ls.size === 0){
    return List([])
  }
  return reverse(ls.shift()).merge(ls.first())
};


/*
  Implement reverse using fold_left
*/

const reverse2 = (ls) => fold_left((acc, el) => acc.unshift(el), List([]), ls)


/*
  Using map and your reverse function, write a function that takes
  a list of strings and returns a list of pairs where each pair contains the
  original string and true if it is a palindrome or false if it is not.
  You may want to use the function reverse_str below in your implementation.
*/
const reverse_str = (str) => reverse(List(str.split(""))).join("");


const palindromes = (ls) => map((el) => {
  const reversed = reverse_str(el)
  return List([el, reversed === el])
}, ls)

const str_ls = List(["test", "testset"]);
const expected = List([List(["test", false]), List(["testset", true])]);

exports.ls_size = ls_size;
exports.sum_sqrs = sum_sqrs;
exports.avg_sqrs = avg_sqrs;
exports.ls_min = ls_min;
exports.ls_max = ls_max;
exports.ls_evens = ls_evens;
exports.ls_odds = ls_odds;
exports.max_even = max_even;
exports.min_even = min_even;
exports.reverse = reverse;
exports.reverse2 = reverse2;
exports.palindromes = palindromes;
