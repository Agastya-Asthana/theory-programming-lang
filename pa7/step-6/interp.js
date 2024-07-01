"use strict";


const Ops = {
  Left: { name: "Left", times: 1 },
  Right: { name: "Right", times: 1 },
  Add: { name: "Add", times: 1 },
  Sub: { name: "Sub", times: 1 },
  LBrack: { name: "LBrack", idx: Infinity },
  RBrack: { name: "RBrack", idx: Infinity },
  Output: { name: "Output" },
  Input: { name: "Input" },
};

const create_program = (bytes) => {
  const prog = [];
  let i = 0;
  while (i < bytes.length) {
    switch (String.fromCharCode(bytes[i])) {
      case "<":
        const leftOp = { ...Ops.Left }
        while (i < bytes.length && String.fromCharCode(bytes[i + 1]) == '<') {
          leftOp.times++
          i++
        }
        prog.push(leftOp)
        break;
      case ">":
        const rightOp = { ...Ops.Right }
        while (i < bytes.length && String.fromCharCode(bytes[i + 1]) == '>') {
          rightOp.times++
          i++
        }
        prog.push(rightOp)
        break;
      case "+":
        const addOp = { ...Ops.Add }
        while (i < bytes.length && String.fromCharCode(bytes[i + 1]) == '+') {
          addOp.times++
          i++
        }
        prog.push(addOp)
        break;
      case "-":
        const subOp = { ...Ops.Sub }
        while (i < bytes.length && String.fromCharCode(bytes[i + 1]) == '-') {
          subOp.times++
          i++
        }
        prog.push(subOp)
        break;
      case "[":
        prog.push({ ...Ops.LBrack })
        break;
      case "]":
        prog.push({ ...Ops.RBrack })
        break;
      case ".":
        prog.push(Ops.Output)
        break;
      case ",":
        prog.push(Ops.Input)
        break;
      default:
        break;
    }
    i += 1;
  }

  return prog;
};

const align_brackets = (prog) => {
  const stack = []
  let i = 0;
  while (i < prog.length) {
    switch (prog[i].name) {
      case Ops.LBrack.name:
        stack.push(i)
        break
      case Ops.RBrack.name:
        const top = stack.pop()
        prog[top].idx = i;
        prog[i].idx = top;
        break
      default:
        break;
    }
    i += 1;
  }
};

const bf_eval = (prog) => {
  const cells = new Uint8Array(10000);
  let cc = 0;
  let pc = 0;

  while (pc < prog.length) {
    switch (prog[pc].name) {
      case Ops.Left.name:
        cc -= prog[pc].times
        break;
      case Ops.Right.name:
        cc += prog[pc].times
        break;
      case Ops.Add.name:
        cells[cc] += prog[pc].times
        break;
      case Ops.Sub.name:
        cells[cc] -= prog[pc].times
        break;
      case Ops.LBrack.name:
        if (cells[cc] == 0) {
          pc = prog[pc].idx
        }
        break;
      case Ops.RBrack.name:
        if (cells[cc] != 0) {
          pc = prog[pc].idx
        }
        break;
      case Ops.Output.name:
        process.stdout.write(String.fromCharCode(cells[cc]));
        break;
      case Ops.Input.name:
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
  const prog = create_program(bytes);
  align_brackets(prog);
  bf_eval(prog);
};

exports.run = run;
