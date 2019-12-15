const { execFileSync, spawnSync } = require("child_process");
const {
  TARGET_FILES,
  COMPILER_CONFIGURATIONS,
  REPETITIONS
} = require("./timing-config");

function compileCompiler() {
  execFileSync("make", [], { stdio: "pipe" });
}

function compileProgram(files, flags) {
  execFileSync("./main.native", [...flags, ...files]);
}

function runProgram() {
  const result = spawnSync("time", ["./a.out"], {
    encoding: "utf-8"
  });
  return result.stderr;
}

compileCompiler();
for (const config of COMPILER_CONFIGURATIONS) {
  for (const target of TARGET_FILES) {
    compileProgram(target, config.flags);
    for (let i = 0; i < REPETITIONS; i++) {
      console.log(
        JSON.stringify({
          config,
          target,
          repetition: i + 1,
          output: runProgram()
        })
      );
    }
  }
}

process.chdir(__dirname);
