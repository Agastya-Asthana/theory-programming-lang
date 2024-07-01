"use strict";
const fs = require("fs");
const hello = fs.readFileSync("bf/hello.bf");
const bench = fs.readFileSync("bf/bench.bf");
const hanoi = fs.readFileSync("bf/hanoi.bf");
const mande = fs.readFileSync("bf/mandelbrot.bf");

const interp1 = require("./step-1/interp.js");
const interp2 = require("./step-2/interp.js");
const interp3 = require("./step-3/interp.js");
const interp4 = require("./step-4/interp.js");
const interp5 = require("./step-5/interp.js");
const interp6 = require("./step-6/interp.js");
const interp7 = require("./step-7/interp.js");

let benchmark = function (method, iterations, bytes) {
  var time = 0;
  var timer = function (action) {
    var d = Date.now();
    if (time < 1 || action === "start") {
      time = d;
      return 0;
    } else if (action === "stop") {
      var t = d - time;
      time = 0;
      return t;
    } else {
      return d - time;
    }
  };

  var i = 0;
  timer("start");
  while (i < iterations) {
    method(bytes);
    i++;
  }

  return timer("stop");
};

let marks = {};
marks["1: hello"] = benchmark(interp1.run, 100, hello);
marks["2: hello"] = benchmark(interp2.run, 100, hello);
marks["3: hello"] = benchmark(interp3.run, 100, hello);
marks["4: hello"] = benchmark(interp4.run, 100, hello);
marks["5: hello"] = benchmark(interp5.run, 100, hello);
marks["6: hello"] = benchmark(interp6.run, 100, hello);
marks["7: hello"] = benchmark(interp7.run, 100, hello);

// marks["1: bench"] = benchmark(interp1.run, 5, bench);
// marks["7: bench"] = benchmark(interp7.run, 5, bench);

marks["1: hanoi"] = benchmark(interp1.run, 1, hanoi);
marks["7: hanoi"] = benchmark(interp7.run, 1, hanoi);

marks["1: mandel"] = benchmark(interp1.run, 1, mande);
marks["7: mandel"] = benchmark(interp7.run, 1, mande);

console.log(marks);
