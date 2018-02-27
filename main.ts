import { Log, logToString } from './log';
import { parse } from './parser';
import { RawType, codeToString } from './ssa';
import { compile } from './compiler';
import { encodeWASM } from './wasm';

declare function require(name: string): any;
declare const WebAssembly: any;

export async function main(): Promise<void> {
  const source = require('fs').readFileSync('example.txt', 'utf8');
  const log: Log = {messages: []};
  const parsed = parse(log, source);
  const code = parsed && compile(log, parsed, RawType.I32);
  (Error as any).stackTraceLimit = Infinity;

  if (log.messages.length > 0) {
    console.log(logToString(source, log));
  } else if (code !== null) {
    console.log(codeToString(code));
    const wasm = encodeWASM(code);
    const {instance} = await WebAssembly.instantiate(wasm);
    console.log(instance);
  } else {
    console.log('done');
  }
}

main().catch(e => setTimeout(() => { throw e; }, 0));
