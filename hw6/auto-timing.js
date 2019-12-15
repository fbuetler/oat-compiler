const fs = require("fs");
const path = require("path");
const { execFileSync, spawnSync } = require("child_process");

const TARGET_FILES = [
  ["atprograms/regalloctest.oat", "runtime.c"],
  ["llprograms/matmul.ll"],
  ["studenttest.oat", "runtime.c"]
];

const COMPILER_CONFIGURATIONS = [
  {
    name: "baseline",
    flags: ["-O1", "--liveness", "trivial", "--regalloc", "none"]
  },
  {
    name: "greedy",
    flags: ["-O1", "--liveness", "dataflow", "--regalloc", "greedy"]
  },
  {
    name: "better",
    flags: ["-O1", "--liveness", "dataflow", "--regalloc", "better"]
  },
  { name: "clang", flags: ["-O1", "--clang"] }
];

const REPETITIONS = 10;

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
