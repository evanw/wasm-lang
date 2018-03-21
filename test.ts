declare const WebAssembly: any;

import { Log, logToString } from './log';
import { parse, Parsed } from './parser';
import { RawType, codeToString } from './ssa';
import { compile } from './compiler';
import { encodeWASM } from './wasm';
import { Imports, library, createImports } from './library';
import { assert } from './util';
import './test.refcount';

export async function run(input: string): Promise<number> {
  const sourceNames = ['<stdin>', '<library>'];
  const sources = [input, library];
  const log: Log = {messages: []};
  const parsed: Parsed = {librarySource: 1, sourceNames, types: [], defs: [], vars: []};
  const imports = createImports();

  for (let i = 0; i < sources.length; i++) {
    parse(log, sources[i], i, parsed);
  }

  if (log.messages.length > 0) {
    throw new Error(logToString(log, sourceNames, sources));
  }

  const code = compile(log, parsed, RawType.I32);

  if (log.messages.length > 0) {
    throw new Error(logToString(log, sourceNames, sources));
  }

  const wasm = encodeWASM(code);
  const {instance} = await WebAssembly.instantiate(wasm, {imports});
  const result = instance.exports.main();
  if (imports.leaks() !== 0) throw new Error(`Leaks: ${imports.leaks()}`);
  return result;
}
