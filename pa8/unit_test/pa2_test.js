/* This file is for testing helpers.js, require-js.js, lazy.js
 * it may help to revisit your initial pa2 submission           */

const { List } = require("immutable");

const test_for_ = (for_) => {
  try {
    let arr = []
    for_(10, (_) => false, (_) => 20, (cur) => arr.push(cur))
    if (arr.length != 0) {
      return false
    }

    arr = []
    for_(0, (el) => el < 3, (el) => el + 1, (el) => arr.push(el))
    if (arr.length != 3) {
      return false
    }
    for (let i = 0; i < 3; i++) {
      if (arr[i] != i) {
        return false
      }
    }

      return true;
  }
  catch (e) {
    return false
  }
};

const test_each = (each) => {
  try {
    const empty = List([])
    each(empty, (el, idx) => null)

    const filled = List([1, 2, 3])
    const res = []
    each(filled, (el, idx) => res.push({el, idx}))
    if (res.length != filled.size) {
      return false
    }
    for (let i = 0; i < 3; i++) {
      if (res[i].el != filled.get(i)) {
        return false
      }
      if (res[i].idx != i) {
        return false
      }
    }

    return true
  }
  catch (e) {
    return false
  }
};


// function for_(cur, cond, next, fbody) {
//   if (cond(cur)) {
//     fbody(cur);
//     for_(next(cur), cond, next, fbody);
//   }
// }

// test.json exists in the auto-grader folder. You can load this file in for testing
// with loadJSONFile("test.json")
// it has the following structure:
// {
//   "key": 10,
//   "key2": 20
// }
const test_loadJSONFile = (loadJSONFile) => {
  try {
    let f = loadJSONFile("test.json")
    if (f.key != 10 || f.key2 != 20) {
      return false
    }
    f.x = 30

    f = loadJSONFile("test.json")
    if (f.x != 30) {
      return false
    }

    return true
  }
  catch (e) {
    return false;
  }
};

exports.test_for_ = test_for_;
exports.test_each = test_each;
exports.test_loadJSONFile = test_loadJSONFile;
