/* This file is for testing quick.js (we won't test extra.js)
 * it may help to revisit your initial pa1 submission         */

const { List } = require("immutable");

const test_fold_left = (fold_left) => {
  try {
    return fl1(fold_left) && fl2(fold_left) && fl3(fold_left);
  }
  catch (e) {
    return false;
  }
};

// verify basic functionality
function fl1(f) {
  const sumList = (ls) => f((acc, x) => acc + x, 0, ls);
  const emptyL = List([]);
  if (sumList(emptyL) != 0) {
    return false;
  }
  const normalL = List([1, 2, 3]);
  if (sumList(normalL) != 6) {
    return false;
  }

  return true;
}

// verify it is folding *left*
function fl2(f) {
  const ordering = [];
  const orderCalled = (ls) =>
    f(
      (_, x) => {
        ordering.push(x);
      },
      0,
      ls,
    );

  const l = List([1, 2, 3]);
  orderCalled(l);
  for (let i = 0; i < l.size; i++) {
    if (ordering[i] != l.get(i)) {
      return false;
    }
  }

  return true;
}

// TODO verify that it is recursive
function fl3(f) {
  return true
}


const fold_left = function (f, base, ls) {
  if (ls.size == 0) {
    return base;
  }

  // Write the recursive fold_left call
  return fold_left(f, f(base, ls.first()), ls.shift());
};

// const it = function (f, base, ls) {
//   for (let i = 0; i < ls.size; i++){
//     base = f(base, ls.get(i))
//   }
//   return base
// };
// const map = (g, ls) => fold_left((lst, el) => lst.push(g(el)), List([]), ls);

const test_map = (map) => {
  try {
    return m1(map)
  }
  catch (e) {
    return false
  }
}

// basic operations
function m1(map){
  const id = (el) => el
  const timesTwo = (el) => el * 2

  const emptyL = List([]);
  const res = map(id, emptyL)
  const res2 = map(id, emptyL)
  if (res.size != 0 || res2.size != 0) {
    return false
  }

  const otherList = List([1, 2, 3])
  const mappedList = map(id, otherList)
  for (let i = 0; i < otherList.size; i++) {
    if (mappedList.get(i) != otherList.get(i)){
      return false
    }
  }

  const doubledList = map(timesTwo, otherList)
  for (let i = 0; i < otherList.size; i++) {
    if (doubledList.get(i) != otherList.get(i) * 2){
      return false
    }
  }

  return true
}

const test_filter = (filter) => {
  try {
    const emptyL = List([]);
    let res = filter((el) => true, emptyL)
    if (res.size != 0) {
      return false
    }

    const otherL = List([1, 2, 3, 4, 5])
    res = filter((el) => el % 2 == 0, otherL)
    if (res.size != 2 || res.get(0) != 2 || res.get(1) != 4) {
      return false
    }

    return true;
  }
  catch(e){
    return false
  }
}


const test_partition = (partition) => {
  try {
    const emptyL = List([]);
    let res = partition((_) => true, emptyL)
    if (res.size != 2 || res.get(0).size != 0 || res.get(1).size != 0) {
      return false
    }

    const otherL = List([1, 2, 3, 4, 5])
    res = partition((el) => el % 2 == 0, otherL)
    if (res.size != 2) {
      return false
    }
    let first = res.get(0)
    let second = res.get(1)
    if (first.size != 2 || first.get(0) != 2 || first.get(1) != 4) {
      return false
    }
    if (second.size != 3 || second.get(0) != 1 || second.get(1) != 3 || second.get(2) != 5) {
      return false
    }

    return true;
  }
  catch (e) {
    return false;
  }
};

const test_quicksort = (quicksort) => {
  try {
    const empty = List([])
    let res = quicksort(empty)
    if (res.size != 0) {
      return false
    }

    const l = List([-1, 5, 0, 1, 3])
    res = quicksort(l)
    if (res.size != l.size) {
      return false
    }
    if (res.get(0) != -1 || res.get(1) != 0 || res.get(2) != 1 || res.get(3) != 3 || res.get(4) != 5) {
      return false
    }

    return true
  }
  catch (e) {
    return false;
  }
};

/* DO NOT MODIFY BELOW THIS LINE */
exports.test_fold_left = test_fold_left;
exports.test_map = test_map;
exports.test_filter = test_filter;
exports.test_partition = test_partition;
exports.test_quicksort = test_quicksort;
