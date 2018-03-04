import { Log, logToString } from './log';
import { parse, Parsed } from './parser';
import { RawType, codeToString } from './ssa';
import { compile } from './compiler';
import { encodeWASM } from './wasm';

declare function require(name: string): any;
declare const WebAssembly: any;
declare const __dirname: string;

const library = `
type int
type bool
type string
`;

export async function main(): Promise<void> {
  const sourceNames = ['(library)', require('path').join(__dirname, 'example.txt')];
  const sources = [library, require('fs').readFileSync('example.txt', 'utf8')];
  const log: Log = {messages: []};
  const parsed: Parsed = {sourceNames, types: [], defs: [], vars: []};
  const wasParsed = sources.every((source, i) => parse(log, source, i, parsed));
  const code = wasParsed ? compile(log, parsed, RawType.I32) : null;
  (Error as any).stackTraceLimit = Infinity;

  if (log.messages.length > 0) {
    console.log(logToString(log, sourceNames, sources));
  } else if (code !== null) {
    console.log(codeToString(code));
    const wasm = encodeWASM(code);
    require('fs').writeFileSync('example.wasm', wasm);
    const {instance} = await WebAssembly.instantiate(wasm);
    console.log(instance);
    const start = Date.now();
    console.log(instance.exports.fib(34));
    console.log('5702887');
    const end = Date.now();
    console.log('time:', ((end - start) / 1000).toFixed(3) + 's');
  } else {
    console.log('done');
  }
}

main().catch(e => setTimeout(() => { throw e; }, 0));
