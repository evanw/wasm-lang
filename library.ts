export const library = `
type int
type bool

@intrinsic("wasm.grow_memory")
def _growMemory(pageCount int) int

@intrinsic("wasm.current_memory")
def _currentMemory() int

@intrinsic("wasm.unreachable")
def _abort()

@import("afterMalloc")
def _afterMalloc(ptr int, size int)

@import("afterFree")
def _afterFree(ptr int, size int)

@import("memCheck")
def _memCheck(ptr int)

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
  _afterMalloc(ptr, size)
  return ptr
}

def _free(ptr int, size int) {
  _afterFree(ptr, size)
}

def _check(ptr int) {
  _memCheck(ptr)
}
`;

export interface Imports {
  leaks(): number;
  afterMalloc(ptr: number, size: number): void;
  afterFree(ptr: number, size: number): void;
  memCheck(ptr: number): void;
}

export function createImports(): Imports {
  const ptrs = new Map<number, number>();
  return {
    leaks(): number {
      return ptrs.size;
    },
    afterMalloc(ptr: number, size: number): void {
      ptrs.set(ptr, size);
    },
    afterFree(ptr: number, size: number): void {
      if (!ptrs.has(ptr)) throw new Error(`Free of invalid pointer ${ptr}`);
      if (ptrs.get(ptr) !== size) throw new Error(`Free of size ${ptr} for allocation of size ${ptrs.get(ptr)}`);
      ptrs.delete(ptr);
    },
    memCheck(ptr: number): void {
      if (!ptrs.has(ptr)) throw new Error(`Use of invalid pointer ${ptr}`);
    },
  };
}
