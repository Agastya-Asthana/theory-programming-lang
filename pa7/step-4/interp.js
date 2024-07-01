"use strict";

const Ops = {
  Left: { name: "Left" },
  Right: { name: "Right" },
  Add: { name: "Add" },
  Sub: { name: "Sub" },
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
        prog.push(Ops.Left)
        break;
      case ">":
        prog.push(Ops.Right)
        break;
      case "+":
        prog.push(Ops.Add)
        break;
      case "-":
        prog.push(Ops.Sub)
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
        console.log(top, i)
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
        cc--
        break;
      case Ops.Right.name:
        cc++
        break;
      case Ops.Add.name:
        cells[cc]++
        break;
      case Ops.Sub.name:
        cells[cc]--
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
