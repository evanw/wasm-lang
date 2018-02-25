import { Log, logToString } from './log';
import { parse } from './parser';
import { RawType } from './ssa';
import { compile } from './compiler';

declare function require(name: string): any;

export function main(): void {
  const fs = require('fs');
  const source = fs.readFileSync('example.txt', 'utf8');
  const log: Log = {messages: []};
  const parsed = parse(log, source);
  const code = parsed && compile(log, parsed, RawType.I32);

  console.log(logToString(source, log));
  if (log.messages.length === 0) {
    // console.log(require('util').inspect(module, {depth: Infinity}));
  }
  console.log('done');
}

main();
