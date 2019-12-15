const fs = require("fs");
const path = require("path");
const {
  TARGET_FILES,
  COMPILER_CONFIGURATIONS,
  REPETITIONS
} = require("./timing-config");

function parseRealTime(s) {
  const [minutes, seconds] = s.split(":");
  return parseInt(minutes) * 60 + parseFloat(seconds);
}

function parseTimeOutput(timeOutput) {
  const m = timeOutput
    .trim()
    .match(
      /^(?:Command exited with non-zero status (\d+)\n)?(\d+\.\d+)user (\d+\.\d+)system (\d+:\d+\.\d+)elapsed \d+%CPU \([^)]+\)k\n\d+inputs\+\d+outputs \(\d+major\+\d+minor\)pagefaults \d+swaps$/
    );
  if (!m) {
    console.warn(timeOutput);
    throw new Error("failed to parse");
  }
  return {
    status: m[1] ? parseInt(m[1]) : 0,
    user: parseFloat(m[2]),
    sys: parseFloat(m[3]),
    real: parseRealTime(m[4])
  };
}

function formatMinutesSeconds(s) {
  const m = Math.floor(s / 60);
  return `${m}m${(s % 60).toFixed(2)}s`;
}

function formatTiming(timing) {
  return ["real", "user", "sys"]
    .map(k => `${k.padEnd(4, " ")}\t${formatMinutesSeconds(timing[k])}`)
    .join("\n");
}

function mergeTimings(a, b, f) {
  const output = {};
  ["real", "user", "sys"].forEach(k => (output[k] = f(a[k], b[k], k)));
  return output;
}

function reconstructCommand({ config: { flags }, target }) {
  return `./main.native ${[...flags, ...target].join(" ")}`;
}

function flatMap(a, f) {
  return [].concat(...a.map(f));
}

process.chdir(__dirname);

const input = fs
  .readFileSync("timing/auto-log.txt", "utf-8")
  .trim()
  .split("\n")
  .map(s => JSON.parse(s));

const averages = new Map();
const sortKeys = new Map();
for (const row of input) {
  const command = reconstructCommand(row);
  const timing = parseTimeOutput(row.output);

  const oldAverage = averages.get(command) || timing;
  averages.set(
    command,
    mergeTimings(oldAverage, timing, (a, b) => (a + b) / 2)
  );

  sortKeys.set(
    command,
    TARGET_FILES.findIndex(
      f => JSON.stringify(f) === JSON.stringify(row.target)
    ) *
      COMPILER_CONFIGURATIONS.length +
      COMPILER_CONFIGURATIONS.findIndex(c => c.name === row.config.name)
  );
}

const sortedCommands = [...averages.keys()].sort(
  (a, b) => sortKeys.get(a) - sortKeys.get(b)
);
const timingOutput = flatMap(sortedCommands, command => [
  "$ " + command,
  "$ time ./a.out",
  "",
  formatTiming(averages.get(command)),
  ""
])
  .join("\n")
  .trim();

const output = [
  `# Timings are averages from ${REPETITIONS} consecutive runs`,
  fs.readFileSync("timing/system.txt", "utf-8").trim(),
  timingOutput
].join("\n\n");

fs.writeFileSync("timinganalysis.txt", output);
