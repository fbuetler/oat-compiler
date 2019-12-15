const NUM_VARS = 4;
const NUM_MIX_ADDS = 150;
const NUM_SUM_ADDS = Math.ceil(NUM_VARS * 1.5);

function times(n, f) {
  return new Array(n).fill(0).map((v, i) => f(i));
}

function randVar() {
  return `x${Math.floor(Math.random() * NUM_VARS)}`;
}

const output = `
int program (int argc, string[] argv) {
${times(NUM_VARS, i => `  var x${i} = argc;`).join('\n')}

  var sum = 0;

  for (var i = 0; i < 100000000; i = i + 1;) {
${times(NUM_MIX_ADDS, i => `    ${randVar()} = ${randVar()} + ${randVar()};`).join('\n')}

${times(NUM_SUM_ADDS, i => `    sum = sum + ${randVar()} + ${randVar()};`).join('\n')}
  }

  return sum;
}
`;

console.log(output.trim());