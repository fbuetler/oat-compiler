const TARGET_FILES = [
  ["atprograms/regalloctest.oat", "runtime.c"],
  ["llprograms/matmul.ll"],
  ["studenttest.oat", "runtime.c"]
];

const COMPILER_CONFIGURATIONS = [
  {
    name: "baseline",
    flags: ["--liveness", "trivial", "--regalloc", "none"]
  },
  {
    name: "greedy",
    flags: ["--liveness", "dataflow", "--regalloc", "greedy"]
  },
  {
    name: "better",
    flags: ["--liveness", "dataflow", "--regalloc", "better"]
  },
  { 
    name: "clang", 
    flags: ["--clang"]
  },
  {
    name: "baseline optimized",
    flags: ["-O1", "--liveness", "trivial", "--regalloc", "none"]
  },
  {
    name: "greedy optimized",
    flags: ["-O1", "--liveness", "dataflow", "--regalloc", "greedy"]
  },
  {
    name: "better optimized",
    flags: ["-O1", "--liveness", "dataflow", "--regalloc", "better"]
  },
  { 
    name: "clang optimized", 
    flags: ["-O1", "--clang"]
  }
];

const REPETITIONS = 10;

module.exports = { TARGET_FILES, COMPILER_CONFIGURATIONS, REPETITIONS };
