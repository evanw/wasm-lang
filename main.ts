import { Log, logToString } from './log';
import { parse, Parsed } from './parser';
import { RawType, codeToString } from './ssa';
import { compile } from './compiler';
import { encodeWASM } from './wasm';

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

@import("afterMalloc")
def _afterMalloc(ptr int)

@import("afterFree")
def _afterFree(ptr int)

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
  _afterMalloc(ptr)
  return ptr
}

def _free(ptr int) {
  _afterFree(ptr)
}
`;

export function run(input: string): {log: string, wasm: Uint8Array | null} {
  const sourceNames = ['<stdin>', '<library>'];
  const sources = [input, library];
  const log: Log = {messages: []};
  const parsed: Parsed = {librarySource: 1, sourceNames, types: [], defs: [], vars: []};

  for (let i = 0; i < sources.length; i++) {
    parse(log, sources[i], i, parsed);
  }

  if (log.messages.length > 0) {
    return {log: logToString(log, sourceNames, sources), wasm: null};
  }

  const code = compile(log, parsed, RawType.I32);

  if (log.messages.length > 0) {
    return {log: logToString(log, sourceNames, sources), wasm: null};
  }

  return {log: '', wasm: encodeWASM(code)};
}
