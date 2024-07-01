"use strict";

const align_brackets = (bytes) => {
  const stack = []
  const bmap = {};
  let i = 0;
  while (i < bytes.length) {
    switch (String.fromCharCode(bytes[i])) {
      case "[":
        stack.push(i)
        break
      case "]":
        const top = stack.pop()
        bmap[top] = i
        bmap[i] = top
        break
      default:
        break;
    }
    i += 1;
  }
  return bmap;
};

const bf_eval = (bytes, bmap) => {
  const cells = new Uint8Array(10000);
  let cc = 0;
  let pc = 0;

  while (pc < bytes.length) {
    const currentChar = String.fromCharCode(bytes[pc])
    switch (currentChar) {
      case "<":
        cc--
        break;
      case ">":
        cc++
        break;
      case "+":
        cells[cc]++
        break;
      case "-":
        cells[cc]--
        break;
      case "[":
        if (cells[cc] == 0) {
          pc = bmap[pc]
        }
        break;
      case "]":
        if (cells[cc] != 0) {
          pc = bmap[pc]
        }
        break;
      case ".":
        process.stdout.write(String.fromCharCode(cells[cc]));
        break;
      case ",":
        cells[cc] = fs.readSync(
          process.stdin.fd,
          Buffer.alloc(1),
          0,
          1,
          null
        )[0];
        break;
      default:
        break;
    }
    pc += 1;
  }
};

const run = (bytes) => {
  const bmap = align_brackets(bytes);
  bf_eval(bytes, bmap);
};

exports.run = run;
