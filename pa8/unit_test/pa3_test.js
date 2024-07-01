/* This file is for testing lazy.js
 * it may help to revisit your initial pa3 submission */

const test_delay = (delay) => {
  try {
    let arr = []
    let mut = -1 
    const f = () => {
      mut++
      arr.push(1)
      return 5 + mut
    }
    let d = delay(f)
    // make sure f hasn't been called yet
    if (arr.length != 0) {
      return false
    }

    let res = d()
    if (res != 5) {
      return false
    }
    res = d()
    res = d()
    res = d()
    res = d()
    res = d()
    res = d()
    // make sure f only gets called once
    if (res != 5) {
      return false
    }
    if (arr.length != 1) {
      return false
    }

    return true
  }
  catch (e) {
    return false
  }
};


function force(t) {
  return t();
}

function delay(f) {
  let called = false;
  let result;
  return function() {
    if (called) {
      return result;
    }
    called = true;
    result = f();
    return result;
  };
}

function enumFrom(n) {
  return delay(function() {
    return {
      head: n,
      tail: enumFrom(n + 1),
    };
  });
}


const test_enumFrom = (enumFrom) => {
  try {
    let e = enumFrom(5)
    if (e().head != 5) {
      return false
    }
    e = e().tail
    if (e().head != 6) {
      return false
    }
    e = e().tail
    if (e().head != 7) {
      return false
    }

    e = enumFrom(-1)
    if (e().head != -1) {
      return false
    }
    e = e().tail
    if (e().head != 0) {
      return false
    }

    return true
  }
  catch (e) {
    return false
  }
};

const test_map = (map) => {
  try {
    const stream = enumFrom(3)
    let m = map((n) => n * 2, stream)
    let first = m()
    if (first.head != 6) {
      return false
    }
    m().tail()
    first.tail()
    let second = first.tail()
    if (second.head != 8) {
      return false
    }

    return true
  }
  catch (e) {
    return false
  }
};

const test_zipWith = (zipWith) => {
  try {
    let s1 = enumFrom(0)
    let s2 = enumFrom(10)
    let zipped = zipWith((e1, e2) => (e1 * 3) - e2, s1, s2)
    if (zipped().head != -10) {
      return false
    }
    let n = zipped().tail
    n()
    if (n().head != -8) {
      return false
    }

    return true
  }
  catch (e) {
    return false
  }
};

const test_tail = (tail) => {
  try {
    let s1 = enumFrom(0)
    let t = tail(s1)
    t()
    if (t().head != 1) {
      return false
    }
    t = t().tail
    t()
    if (t().head != 2) {
      return false
    }

    return true
  }
  catch (e) {
    return false
  }
};

const test_cons = (cons) => {
  try {
    let s1 = enumFrom(0)
    let s2 = cons(22, s1)
    if (s2().head != 22) {
      return false
    }
    let next = s2().tail
    if ((next().head) != 0) {
      return false
    }

    return true
  }
  catch (e) {
    return false
  }
};

exports.test_delay = test_delay;
exports.test_enumFrom = test_enumFrom;
exports.test_map = test_map;
exports.test_zipWith = test_zipWith;
exports.test_tail = test_tail;
exports.test_cons = test_cons;
