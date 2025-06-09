import { assert } from "./util";
import { run } from "./test";
import { describe, it } from "node:test";

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

describe('match if', () => {
  it('basic match 1', async () => {
    assert(21 === await run(`
      type Foo {
        A
        B
      }
      def foo(foo Foo) int {
        if var A() = foo { return 1 }
        else if var B() = foo { return 20 }
        else { return 300 }
      }
      @export("main")
      def main() int {
        return foo(A) + foo(B)
      }
    `));
  });

  it('basic match 2', async () => {
    assert(21 === await run(`
      type Foo {
        A
        B
      }
      def foo(foo Foo) int {
        if var A() = foo { return 1 }
        if var B() = foo { return 20 }
        return 300
      }
      @export("main")
      def main() int {
        return foo(A) + foo(B)
      }
    `));
  });

  it('content match 1', async () => {
    assert(21 === await run(`
      type Foo {
        A
        B(x int)
      }
      def foo(foo Foo) int {
        if var A() = foo { return 1 }
        else if var B(x) = foo { return x }
        else { return 300 }
      }
      @export("main")
      def main() int {
        return foo(A) + foo(B(20))
      }
    `));
  });

  it('content match 2', async () => {
    assert(21 === await run(`
      type Foo {
        A
        B(x int)
      }
      def foo(foo Foo) int {
        if var A() = foo { return 1 }
        if var B(x) = foo { return x }
        return 300
      }
      @export("main")
      def main() int {
        return foo(A) + foo(B(20))
      }
    `));
  });

  it('ctor match 1', async () => {
    assert(1 === await run(`
      type Foo {
        A
        B(x int)
      }
      @export("main")
      def main() int {
        if var B(x) = B(1) {
          return x
        } else {
          return 2
        }
      }
    `));
  });

  it('ctor match 2', async () => {
    assert(1 === await run(`
      type Foo {
        A
        B(x int)
      }
      @export("main")
      def main() int {
        if var B(x) = B(1) {
          return x
        }
        return 2
      }
    `));
  });

  it('linked list', async () => {
    assert(45 === await run(`
      type List {
        Empty
        Link(head int, tail List)
      }

      @export("main")
      def main() int {
        var list = Empty

        var i = 0
        while i < 10 {
          list = Link(i, list)
          i = i + 1
        }

        var total = 0
        while var Link(hd, tl) = list {
          total = total + hd
          list = tl
        }

        return total
      }
    `));
  });
});
