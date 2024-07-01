"use strict";

const bf_eval = (bytes) => {
  const cells = new Uint8Array(10000);
  let cc = 0;
  let pc = 0;
  let stack = [];

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
          let bracketCount = 1
          while (bracketCount > 0) {
            pc++
            if (String.fromCharCode(bytes[pc]) === "[") {
              bracketCount++
            } else if (String.fromCharCode(bytes[pc]) === "]") {
              bracketCount--
            }
          }
        } else {
          stack.push(pc)
        }
        break;
      case "]":
        if (cells[cc] != 0) {
          pc = stack[stack.length - 1]
        } else {
          stack.pop()
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
  bf_eval(bytes);
};

exports.run = run
