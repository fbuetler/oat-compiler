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

module.exports = { TARGET_FILES, COMPILER_CONFIGURATIONS, REPETITIONS };
