import { assert } from "./util";
import { run } from "./test";

declare const describe: any;
declare const it: any;

describe('return void', () => {
  it('bare ctor', async () => {
    assert(undefined === await run(`
      type Foo
      @export("main")
      def main() {
        Foo
      }
    `));
  });

  it('local var', async () => {
    assert(undefined === await run(`
      type Foo
      @export("main")
      def main() {
        var foo = Foo
      }
    `));
  });

  it('local assign', async () => {
    assert(undefined === await run(`
      type Foo
      @export("main")
      def main() {
        var foo = Foo
        foo = Foo
      }
    `));
  });
});

describe('return int', () => {
  it('bare ctor', async () => {
    assert(1 === await run(`
      type Foo
      @export("main")
      def main() int {
        Foo
        return 1
      }
    `));
  });

  it('local var', async () => {
    assert(1 === await run(`
      type Foo
      @export("main")
      def main() int {
        var foo = Foo
        return 1
      }
    `));
  });

  it('local assign', async () => {
    assert(1 === await run(`
      type Foo
      @export("main")
      def main() int {
        var foo = Foo
        foo = Foo
        return 1
      }
    `));
  });
});

describe('return type', () => {
  it('bare call', async () => {
    assert(undefined === await run(`
      type Foo
      def foo() Foo {
        return Foo
      }
      @export("main")
      def main() {
        foo()
      }
    `));
  });

  it('local var', async () => {
    assert(undefined === await run(`
      type Foo
      def foo() Foo {
        return Foo
      }
      @export("main")
      def main() {
        var bar = foo()
      }
    `));
  });

  it('local assign 1', async () => {
    assert(undefined === await run(`
      type Foo
      def foo() Foo {
        return Foo
      }
      @export("main")
      def main() {
        var bar = foo()
        bar = Foo
      }
    `));
  });

  it('local assign 2', async () => {
    assert(undefined === await run(`
      type Foo
      def foo() Foo {
        return Foo
      }
      @export("main")
      def main() {
        var bar = Foo
        bar = foo()
      }
    `));
  });
});


describe('args', () => {
  it('arg', async () => {
    assert(undefined === await run(`
      type Foo
      def foo(foo Foo) {
      }
      @export("main")
      def main() {
        foo(Foo)
      }
    `));
  });

  it('arg assign', async () => {
    assert(undefined === await run(`
      type Foo
      def foo(foo Foo) {
        foo = Foo
      }
      @export("main")
      def main() {
        foo(Foo)
      }
    `));
  });

  it('return arg', async () => {
    assert(undefined === await run(`
      type Foo
      def foo(foo Foo) Foo {
        return foo
      }
      @export("main")
      def main() {
        foo(Foo)
      }
    `));
  });

  it('return arg assign', async () => {
    assert(undefined === await run(`
      type Foo
      def foo(foo Foo) Foo {
        foo = Foo
        return foo
      }
      @export("main")
      def main() {
        foo(Foo)
      }
    `));
  });
});

describe('nested', () => {
  it('bare ctor', async () => {
    assert(undefined === await run(`
      type Foo {
        Bar
        Baz(foo Foo)
      }
      @export("main")
      def main() {
        Baz(Bar)
      }
    `));
  });

  it('local var', async () => {
    assert(undefined === await run(`
      type Foo {
        Bar
        Baz(foo Foo)
      }
      @export("main")
      def main() {
        var foo = Baz(Bar)
      }
    `));
  });

  it('local assign 1', async () => {
    assert(undefined === await run(`
      type Foo {
        Bar
        Baz(foo Foo)
      }
      @export("main")
      def main() {
        var foo = Baz(Bar)
        foo = Bar
      }
    `));
  });

  it('local assign 2', async () => {
    assert(undefined === await run(`
      type Foo {
        Bar
        Baz(foo Foo)
      }
      @export("main")
      def main() {
        var foo = Bar
        foo = Baz(Bar)
      }
    `));
  });
});
