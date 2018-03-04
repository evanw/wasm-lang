export function assert(truth: boolean): void {
  if (!truth) {
    throw new Error('Assertion failed');
  }
}

export function align(value: number, align: number): number {
  assert((align & (align - 1)) === 0); // Alignment must be a power of 2
  value = (value + align - 1) & ~(align - 1);
  return value;
}
