const Parser = require("tree-sitter");
const JavaScript = require("tree-sitter-javascript");

const parser = new Parser();
parser.setLanguage(JavaScript);

/*
 * Optional helper function:
 * Return a list of all the parameters used in the function
 * defintion.
 */


const find_parameter_names = (func_ast) => {
  func_ast = func_ast.rootNode
  const result = []

  const query_str = `(function_declaration
    parameters: (formal_parameters (identifier) @f)
  )`;
  const query = new Parser.Query(JavaScript, query_str);
  query.matches(func_ast).forEach((match) => {
    const id = match.captures[0].node;
    result.push(id.text)
  });

  return result
};

/*
 * Optional helper function:
 * Find all conditional expressions in the function which
 * depend on the given variable name
 */
const find_conditional_expressions = (func_ast, variable_name) => {
  func_ast = func_ast.rootNode
  const result = []

  const q = `(if_statement
	condition: (parenthesized_expression) @p
)`;
  const query = new Parser.Query(JavaScript, q);

  const q2 = `(identifier) @i`;
  const query2 = new Parser.Query(JavaScript, q2);

  query.matches(func_ast).forEach((match) => {
    const parenExpr = match.captures[0].node
    query2.matches(parenExpr).forEach((m) => {
      const varName = m.captures[0].node.text
      if (varName === variable_name) {
        result.push(parenExpr)
      }
    })
  });

  return result;
};

/*
 * Create a map from variable name to possible options
 * based on a conditional expression
 * required boundaries:
 *  comparison with a string (arg == "hello") (8points)
 *  comparison with a number (arg > 10) (8points)
 *  comparison with a binary operation (arg > 10 + 5) (4points)
 *  comparison with another argument (arg1 > arg2) (4points)
 *  beta-substitution (arg > x) [x := 10] (4points)
 *  complex analysis (4points)
 */
const get_decisions = (func_ast, variable_name, conditionals) => {
  const result = []

  const stringQ = `(parenthesized_expression 
        [
          (binary_expression
            (identifier) @id
            operator: "=="
            (string) @str
          )
          (binary_expression
            (string) @str
            operator: "=="
            (identifier) @id
          )
        ]
)`;
  const numberComparisonQ = `(parenthesized_expression 
        [
          (binary_expression
            (identifier) @id
            operator: [">" "<" "<=" ">="] @op
            (number) @num
          )
          (binary_expression
            (number) @num
            operator: [">" "<" "<=" ">="] @op
            (identifier) @id
          )
        ]
)`
  const binaryOpQ = `(parenthesized_expression 
  [
    (binary_expression
      (identifier) @id
      operator: [">" "<" "<=" ">="]
      (binary_expression
          left: (number) @left
          operator: _ @operation
          right: (number) @right
      )
    )
    (binary_expression
      (binary_expression
          left: (number) @left
          operator: _ @operation
          right: (number) @right
      )
      operator: [">" "<" "<=" ">="]
      (identifier) @id
    )
  ]
)`

  for (let cond of conditionals) {
    // string comparisons
    const stringQuery = new Parser.Query(JavaScript, stringQ);
    stringQuery.matches(cond).forEach((match) => {
      const id = match.captures.filter((node) => node.name == "id")[0].node
      const str = match.captures.filter((node) => node.name == "str")[0].node
      if (id.text === variable_name) {
        let stringText = str.text
        stringText = stringText.substring(1, stringText.length - 1)
        result.push({"string": stringText});
      }
    })

    // number comparisons
    const numberQuery = new Parser.Query(JavaScript, numberComparisonQ);
    numberQuery.matches(cond).forEach((match) => {
      const caps = match.captures
      const id = caps.filter((node) => node.name == "id")[0].node
      const num = caps.filter((node) => node.name == "num")[0].node
      const _op = caps.filter((node) => node.name == "op")[0].node
      if (id.text === variable_name) {
        result.push({"number": Number(num.text)})
      }
    })

    // binary op comparison (i assume it's two numbers)
    const binaryOpQuery = new Parser.Query(JavaScript, binaryOpQ);
    binaryOpQuery.matches(cond).forEach((match) => {
      const caps = match.captures
      const id = caps.filter((node) => node.name == "id")[0].node
      const left = caps.filter((node) => node.name == "left")[0].node
      const right = caps.filter((node) => node.name == "right")[0].node
      const op = caps.filter((node) => node.name == "operation")[0].node
      if (id.text === variable_name) {
        // TODO, i could run an eval on the entire binary expression instead?
        const val = eval(left.text + op.text + right.text)
        result.push({"binaryOp": val})
      }
    })
  }
  return result
};

const test_evaluation = (func_ast, parameter_values) => {
  let passed;
  try {
    eval(
      `(${func_ast.rootNode.text})(${Object.values(parameter_values).join(
        ", ",
      )})`,
    );
    passed = true;
  } catch (e) {
    passed = false;
  }

  return passed;
};

function generator(vars, data) {
  const builder = []
  let idx = 0

  for (let v of vars) {
    const arr = []
    builder.push(arr)

    const opts = data[v]
    for (let opt of opts) {
      if ('binaryOp' in opt) {
        const val = opt.binaryOp
        builder[idx].push(val - 1)
        builder[idx].push(val + 1)
      }
      else if ('string' in opt) {
        const val = opt.string
        builder[idx].push(val)
        builder[idx].push(val + "cat")
      }
      else if ('number' in opt) {
        const val = opt.number
        builder[idx].push(val - 1)
        builder[idx].push(val + 1)
      }
    }
    idx++
    if (arr.length == 0) {
      arr.push("null")
    }
  }

  return generatePermutations(builder)
}

function generatePermutations(arrays) {
    if (arrays.length === 1) {
        return arrays[0].map(element => [element]);
    }

    const subPermutations = generatePermutations(arrays.slice(1));
    
    const result = [];
    arrays[0].forEach(element => {
        subPermutations.forEach(subPermutation => {
            result.push([element, ...subPermutation]);
        });
    });

    return result;
}

const detect_boundary = (func_ast) => {
  // Below is an example structure for this function.
  // You may choose to use it or create your own, as long as you follow the interface:
  // ast -> { pass: {args}, fail: {args}}

  // Start by finding parameters
  const parameter_names = find_parameter_names(func_ast);

  const data = {}
  for (const parameter_name of parameter_names) {
    // Find conditional expressions that depend on the parameter
    const conditional_expressions = find_conditional_expressions(
      func_ast,
      parameter_name,
    );

    // For each conditional expression, create a map from variable name to possible options
    const decisions = get_decisions(
      func_ast,
      parameter_name,
      conditional_expressions,
    );
    data[parameter_name] = decisions
  }
  const opts = generator(parameter_names, data)

  let results = {
    pass: false,
    fail: false,
  };

  for (const pv of opts) {
    const paramValues = {}
    let idx = 0
    for (const v of pv) {
      paramValues[parameter_names[idx]] = v
      idx++
    }
    let passed = test_evaluation(func_ast, paramValues);
    if (passed && !results["pass"]) {
      results["pass"] = paramValues
    } else if (!passed && !results["fail"]) {
      results["fail"] = paramValues
    }
  }

  console.log(opts)

  return results;
};

const exampleSourceCode =`function name(a, b) {
    if (a > 3) {
      throw "hi"
    }
    return 5
}`;

const tree = parser.parse(exampleSourceCode);
console.log(detect_boundary(tree))

exports.detect_boundary = detect_boundary;
