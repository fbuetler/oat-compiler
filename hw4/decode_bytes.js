#!/usr/bin/env node

const fs = require("fs");

const bytes = fs
  .readFileSync(0)
  .toString()
  .trim()
  .split(/\s+/)
  .map(v => +v);
process.stdout.write(Buffer.from(bytes));
