"use strict";
const fs = require("fs");

// TODO: change the "step-{n}" to validate each of your impelmentations
const interp = require("./step-7/interp.js");

const hello = fs.readFileSync("bf/hello.bf");
const bench = fs.readFileSync("bf/bench.bf");

interp.run(hello);
interp.run(bench);
