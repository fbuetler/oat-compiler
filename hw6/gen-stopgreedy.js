const NUM_VARS = 4;
const NUM_MIX_ADDS = 150;

function times(n, f) {
  return new Array(n).fill(0).map((v, i) => f(i));
}

function randVar() {
  return `x${Math.floor(Math.random() * NUM_VARS)}`;
}

const output = `
int program (int argc, string[] argv) {
${times(NUM_VARS, i => `  var x${i} = argc;`).join("\n")}

  var sum = 0;

  for (var i = 0; i < 20000000; i = i + 1;) {
${times(
  NUM_MIX_ADDS,
  () => `    ${randVar()} = ${randVar()} + ${randVar()};`
).join("\n")}

${times(NUM_VARS, i => `    sum = x${i} + sum + x${i};`).join("\n")}
  }

  if (sum > 0) {
    return 0;
  } else {
    return sum;
  }
}
`;

console.log(output.trim());
