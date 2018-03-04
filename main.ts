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

@intrinsic("wasm.grow_memory")
def _growMemory(pageCount int) int

@intrinsic("wasm.current_memory")
def _currentMemory() int

@intrinsic("wasm.unreachable")
def _abort()

var _ptr = 0
var _end = 0

def _malloc(size int) int {
  if _ptr + size > _end {
    # Lazily initialize
    if _end == 0 {
      _end = _currentMemory() * (1 << 16)
      _ptr = _end
    }

    # Ask for more pages
    var pages = (_ptr + size - _end + (1 << 16) - 1) >> 16
    if _growMemory(pages) == -1 {
      _abort()
    }
    _end = _end + pages * (1 << 16)
  }

  # Use a bump allocator
  var ptr = _ptr
  _ptr = (_ptr + size + 7) & ~7
  return ptr
}
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

    const {instance} = await WebAssembly.instantiate(wasm, {imports: {
      add: (a: number, b: number) => a + b,
    }});
    console.log(instance);

    const start = Date.now();
    console.log(instance.exports.fib(34));
    console.log('5702887');
    const end = Date.now();
    console.log('time:', ((end - start) / 1000).toFixed(3) + 's');

    console.log(instance.exports.main());
    console.log(instance.exports.add(1, 2));
  } else {
    console.log('done');
  }
}

main().catch(e => setTimeout(() => { throw e; }, 0));
