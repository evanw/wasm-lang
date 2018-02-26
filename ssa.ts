import { assert } from './util';

export interface InsRef {
  // If this is negative, this is the constant stored at constants[~index].
  // Otherwise this is the index of a previous instruction in this block.
  index: number;
}

export enum RawType {
  Void,
  I32,
  I64,
}

export type Ins =
  {kind: 'Nop'} |
  {kind: 'Alias', value: InsRef, type: RawType} |
  {kind: 'Call', index: number, args: InsRef[], retType: RawType} |

  {kind: 'PtrGlobal', index: number} |
  {kind: 'PtrStack', index: number} |

  {kind: 'MemAlloc', size: InsRef} |
  {kind: 'MemFree', ptr: InsRef, size: InsRef} |
  {kind: 'MemCopy', from: InsRef, to: InsRef, size: number, align: number} |

  {kind: 'MemGet8', ptr: InsRef, offset: number} |
  {kind: 'MemSet8', ptr: InsRef, offset: number, value: InsRef} |
  {kind: 'MemGet32', ptr: InsRef, offset: number} |
  {kind: 'MemSet32', ptr: InsRef, offset: number, value: InsRef} |

  {kind: 'LocalGet', local: number} |
  {kind: 'LocalSet', local: number, value: InsRef} |

  {kind: 'Retain', ptr: InsRef} |
  {kind: 'Release', ptr: InsRef, dtor: number} |

  {kind: 'Eq32', left: InsRef, right: InsRef} |
  {kind: 'NotEq32', left: InsRef, right: InsRef} |
  {kind: 'Lt32S', left: InsRef, right: InsRef} |
  {kind: 'Lt32U', left: InsRef, right: InsRef} |
  {kind: 'LtEq32S', left: InsRef, right: InsRef} |
  {kind: 'LtEq32U', left: InsRef, right: InsRef} |

  {kind: 'Add32', left: InsRef, right: InsRef} |
  {kind: 'Sub32', left: InsRef, right: InsRef} |
  {kind: 'Mul32', left: InsRef, right: InsRef} |
  {kind: 'Div32S', left: InsRef, right: InsRef} |
  {kind: 'Div32U', left: InsRef, right: InsRef};

// Unlike in other compilers, BasicBlocks form a tree instead of a block soup.
// This makes it possible to emit the structured output that WebAssembly
// requires. Reverse-engineering the structure from the block soup is possible
// but is unnecessary if the compiler preserves the structure in the first
// place.
//
// This structure preservation is done by restricting jump targets to either
// the next block, a child block, the next block of a parent, or a loop header.
export type Jump =
  {kind: 'ReturnVoid'} |
  {kind: 'Return', value: InsRef} |

  // This is the initial jump value for newly-created blocks. It's used to
  // detect missing jumps when the user forgets a return statement.
  {kind: 'Missing'} |

  // These instructions allow for branching to a child, a parent, or to this
  // block (forming a loop).
  {kind: 'Goto', target: JumpTarget} |
  {kind: 'Branch', value: InsRef, yes: JumpTarget, no: JumpTarget};

type JumpTarget =
  // This jumps to a child, which means this block is the child block's parent.
  {kind: 'Child', index: number} |

  // This jumps to the "next" field of the parent with the provided index. The
  // parent must be currently on the stack during the DFS from the entry point
  // to this block.
  {kind: 'Next', parent: number} |

  // This jumps back to the parent with the provided index. This is the only
  // jump that forms a back edge.
  {kind: 'Loop', parent: number};

export interface BasicBlock {
  insList: Ins[];
  jump: Jump;

  // This is the index of the next basic block in the basic block tree. It will
  // be -1 if there's no such block (e.g. if this is the last branch inside a
  // loop body). Note that control flow doesn't necessarily transfer into this
  // block at all (e.g. a loop body with a return statement).
  next: number;

  // This maps the index of a local to the InsRef for this block that is known
  // to already hold the value for that local. That way we can avoid needlessly
  // reloading the same local over and over within a given block. This will be
  // updated whenever a "LocalGet" or "LocalSet" instruction is needed.
  previousLocals: Map<number, InsRef>;

  // This maps an InsRef for this block to the index of a local variable. It's
  // used to "spill" values to a temporary local variable when they are needed
  // in another block. Adding an instruction returns a ValueRef, which contains
  // the block index. When that ValueRef is later turned into an InsRef for
  // another block via unwrapRef, a local variable is allocated if one hasn't
  // already been allocated and the variable is spilled in the source block and
  // loaded in the target block. This map tracks the spill so it can be reused
  // if it's needed again in the future instead of generating another one.
  spills: Map<number, number>;
}

export interface Func {
  name: string;
  argTypes: RawType[];
  retType: RawType;
  blocks: BasicBlock[];

  // This is different for 32-bit vs. 64-bit
  ptrType: RawType;

  // This stores any constants indexed by "InsRef"
  constants: number[];

  // This stores the type of each local (used by the "LocalGet" and "LocalSet" instructions)
  locals: RawType[];

  // This stores the byte size of each stack slot (used by the "PtrStack" instruction)
  stack: number[];
}

export interface Code {
  funcs: Func[];

  // This stores the byte size of each global slot (used by the "PtrGlobal" instruction)
  globals: number[];
}

export function createFunc(name: string, ptrType: RawType): Func {
  return {
    name,
    argTypes: [],
    retType: RawType.Void,
    ptrType,
    blocks: [],
    constants: [0],
    locals: [],
    stack: [],
  };
}

export function createBlock(func: Func): number {
  func.blocks.push({
    insList: [],
    jump: {kind: 'Missing'},
    next: -1,
    previousLocals: new Map(),
    spills: new Map(),
  });
  return func.blocks.length - 1;
}

export function setJump(func: Func, block: number, jump: Jump): void {
  func.blocks[block].jump = jump;
}

export function setNext(func: Func, block: number, next: number): void {
  assert(next >= -1 && next < func.blocks.length);
  func.blocks[block].next = next;
}

export interface ValueRef {
  ref: InsRef;

  // The index of the block this ref is from, will be -1 for constants
  block: number;
}

export function createLocal(func: Func, type: RawType): number {
  func.locals.push(type);
  return func.locals.length - 1;
}

export function createConstant(func: Func, value: number): ValueRef {
  if (value === 0) return {block: -1, ref: {index: -1}};
  func.constants.push(value);
  return {block: -1, ref: {index: -func.constants.length}};
}

export function unwrapRef(func: Func, block: number, value: ValueRef): InsRef {
  if (value.block === block || value.block < 0) return value.ref;
  const local = spillToLocal(func, value.block, value.ref);
  return addLocalGet(func, block, local).ref;
}

export function addIns(func: Func, block: number, ins: Ins): ValueRef {
  const insList = func.blocks[block].insList;
  const index = insList.length;
  insList.push(ins);
  return {block, ref: {index}};
}

export function addLocalGet(func: Func, block: number, local: number): ValueRef {
  const target = func.blocks[block];
  let ref = target.previousLocals.get(local);
  if (ref === undefined) {
    ref = addIns(func, block, {kind: 'LocalGet', local}).ref;
    target.previousLocals.set(local, ref);
  }
  return {block, ref};
}

export function addLocalSet(func: Func, block: number, local: number, value: ValueRef): ValueRef {
  const target = func.blocks[block];
  const ref = unwrapRef(func, block, value);
  target.previousLocals.set(local, ref);
  return addIns(func, block, {kind: 'LocalSet', local, value: ref});
}

function refToString(func: Func, ref: InsRef): string {
  if (ref.index >= 0) return `t${ref.index}`;
  return func.constants[~ref.index].toString();
}

function blockToString(context: ToStringContext, block: BasicBlock, indent: string): string {
  let text = '';

  for (let i = 0; i < block.insList.length; i++) {
    const ins = block.insList[i];

    switch (ins.kind) {
      case 'Nop':
        break;

      case 'Alias':
        text += `${indent}t${i} = ${refToString(context.func, ins.value)}\n`;
        break;

      case 'Call': {
        const args = [context.code.funcs[ins.index].name];
        for (const arg of ins.args) {
          args.push(refToString(context.func, arg));
        }
        text += `${indent}t${i} = call ${args.join(', ')}\n`;
        break;
      }

      case 'PtrGlobal':
      case 'PtrStack':
      case 'MemAlloc':
      case 'MemFree':
      case 'MemCopy':
      case 'MemGet8':
      case 'MemSet8':
      case 'MemGet32':
      case 'MemSet32':
        break;

      case 'LocalGet':
        text += `${indent}t${i} = v${ins.local}\n`;
        break;

      case 'LocalSet':
        text += `${indent}v${ins.local} = ${refToString(context.func, ins.value)}\n`;
        break;

      case 'Retain':
        text += `${indent}retain ${refToString(context.func, ins.ptr)}\n`;
        break;

      case 'Release':
        text += `${indent}release ${refToString(context.func, ins.ptr)}, ${context.code.funcs[ins.dtor].name}\n`;
        break;

      case 'Eq32':
        text += `${indent}t${i} = i32.eq ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'NotEq32':
        text += `${indent}t${i} = i32.ne ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'Lt32S':
        text += `${indent}t${i} = i32.lt_s ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'Lt32U':
        text += `${indent}t${i} = i32.lt_u ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'LtEq32S':
        text += `${indent}t${i} = i32.lte_s ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'LtEq32U':
        text += `${indent}t${i} = i32.lte_u ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'Add32':
        text += `${indent}t${i} = i32.add ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'Sub32':
        text += `${indent}t${i} = i32.sub ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'Mul32':
        text += `${indent}t${i} = i32.mul ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'Div32S':
        text += `${indent}t${i} = i32.div_s ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      case 'Div32U':
        text += `${indent}t${i} = i32.div_u ${refToString(context.func, ins.left)}, ${refToString(context.func, ins.right)}\n`;
        break;

      default: {
        const checkCovered: void = ins;
        throw new Error('Internal error');
      }
    }
  }

  return text;
}

interface ToStringContext {
  code: Code;
  func: Func;
  stack: number[];
}

export function codeToString(code: Code): string {
  let text = '';

  for (const func of code.funcs) {
    const context: ToStringContext = {
      code,
      func,
      stack: [],
    };
    const args = func.argTypes.map((arg, i) => `v${i} ${RawType[arg]}`).join(', ');
    text += `def ${func.name}(${args}) ${RawType[func.retType]}\n`;
    for (let i = 0; i < func.locals.length; i++) {
      text += `  v${i} ${RawType[func.locals[i]]}\n`;
    }
    text += blockTreeToString(context, 0, '  ');
  }

  return text;
}

function blockTreeToString(context: ToStringContext, index: number, indent: string): string {
  let text = '';

  while (index !== -1) {
    const block = context.func.blocks[index];
    context.stack.push(index);
    text += `${indent}l${context.stack.length - 1}: {\n`;
    indent += '  ';
    text += `${indent}# b${index}\n`;
    text += blockToString(context, block, indent);

    switch (block.jump.kind) {
      case 'Missing':
        text += `${indent}(missing jump)\n`;
        break;

      case 'ReturnVoid':
        text += `${indent}return\n`;
        break;

      case 'Return':
        text += `${indent}return ${refToString(context.func, block.jump.value)}\n`;
        break;

      case 'Goto': {
        text += jumpTargetToString(context, block.jump.target, indent);
        break;
      }

      case 'Branch': {
        text += `${indent}if ${refToString(context.func, block.jump.value)}\n`;
        text += jumpTargetToString(context, block.jump.yes, indent + '  ');
        text += `${indent}else\n`;
        text += jumpTargetToString(context, block.jump.no, indent + '  ');
        break;
      }

      default: {
        const checkCovered: void = block.jump;
        throw new Error('Internal error');
      }
    }

    indent = indent.slice(2);
    text += `${indent}}\n`;
    context.stack.pop();
    index = block.next;
  }

  return text;
}

function jumpTargetToString(context: ToStringContext, target: JumpTarget, indent: string): string {
  switch (target.kind) {
    case 'Child':
      return blockTreeToString(context, target.index, indent);

    case 'Next': {
      const index = context.stack.indexOf(target.parent);
      assert(index !== -1);
      return `${indent}break l${index} # b${target.parent}\n`;
    }

    case 'Loop': {
      const index = context.stack.indexOf(target.parent);
      assert(index !== -1);
      return `${indent}continue l${index} # b${target.parent}\n`;
    }

    default: {
      const checkCovered: void = target;
      throw new Error('Internal error');
    }
  }
}

function spillToLocal(func: Func, block: number, ref: InsRef): number {
  assert(ref.index >= 0);
  const spills = func.blocks[block].spills;
  const spill = spills.get(ref.index);
  if (spill !== undefined) return spill;
  const local = createLocal(func, typeOf(func, block, ref));
  addLocalSet(func, block, local, {block, ref});
  spills.set(ref.index, local);
  return local;
}

function typeOf(func: Func, block: number, ref: InsRef): RawType {
  const target = func.blocks[block];
  const ins = target.insList[ref.index];

  switch (ins.kind) {
    case 'LocalGet':
      return func.locals[ins.local];

    case 'Nop':
    case 'MemFree':
    case 'MemCopy':
    case 'MemSet8':
    case 'MemSet32':
    case 'LocalSet':
    case 'Retain':
    case 'Release':
      return RawType.Void;

    case 'PtrGlobal':
    case 'PtrStack':
    case 'MemAlloc':
      return func.ptrType;

    case 'Alias':
      return ins.type;

    case 'Call':
      return ins.retType;

    case 'MemGet8':
    case 'MemGet32':
    case 'Eq32':
    case 'NotEq32':
    case 'Lt32S':
    case 'Lt32U':
    case 'LtEq32S':
    case 'LtEq32U':
    case 'Add32':
    case 'Sub32':
    case 'Mul32':
    case 'Div32S':
    case 'Div32U':
      return RawType.I32;
  }

  throw new Error('Internal error');
}
