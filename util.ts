export function assert(truth: boolean): void {
  if (!truth) {
    throw new Error('Assertion failed');
  }
}
