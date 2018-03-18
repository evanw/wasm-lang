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

export enum BinIns {
  Eq32,
  NotEq32,
  Lt32S,
  Lt32U,
  LtEq32S,
  LtEq32U,
  And32,
  Or32,
  Xor32,
  Add32,
  Sub32,
  Mul32,
  Div32S,
  Div32U,
  Shl32,
  Shr32S,
  Shr32U,
}

export type Ins =
  {kind: 'Nop'} |

  {kind: 'Call', index: number, args: InsRef[], retType: RawType} |
  {kind: 'CallImport', index: number, args: InsRef[], retType: RawType} |
  {kind: 'CallIntrinsic', name: string, args: InsRef[], retType: RawType} |

  {kind: 'PtrGlobal', index: number} |
  {kind: 'PtrStack', index: number} |

  {kind: 'MemAlloc', size: InsRef} |
  {kind: 'MemFree', ptr: InsRef, size: InsRef} |

  {kind: 'MemGet8', ptr: InsRef, offset: number} |
  {kind: 'MemSet8', ptr: InsRef, offset: number, value: InsRef} |
  {kind: 'MemGet32', ptr: InsRef, offset: number} |
  {kind: 'MemSet32', ptr: InsRef, offset: number, value: InsRef} |

  {kind: 'LocalGet', local: number} |
  {kind: 'LocalSet', local: number, value: InsRef} |

  {kind: 'Retain', ptr: InsRef} |
  {kind: 'Release', ptr: InsRef, dtor: number} |

  {kind: 'Binary', op: BinIns, left: InsRef, right: InsRef};

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

export type JumpTarget =
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
  // be null if there's no such block (e.g. if this is the last branch inside a
  // loop body). Note that control flow doesn't necessarily transfer into this
  // block at all (e.g. a loop body with a return statement).
  next: number | null;

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
  exportName: string | null;
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

export interface Global {
  name: string;
  bytes: Uint8Array;
}

export interface Import {
  name: string;
  argTypes: RawType[];
  retType: RawType;
}

export interface Code {
  funcs: Func[];
  globals: Global[];
  imports: Import[];
  mallocIndex: number | null;
  freeIndex: number | null;
}

export function createCode(): Code {
  return {
    funcs: [],
    globals: [],
    imports: [],
    mallocIndex: null,
    freeIndex: null,
  };
}

export function createFunc(name: string, ptrType: RawType): Func {
  return {
    name,
    exportName: null,
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
    next: null,
    previousLocals: new Map(),
    spills: new Map(),
  });
  return func.blocks.length - 1;
}

export function setJumpGoto(func: Func, block: number, target: JumpTarget): void {
  func.blocks[block].jump = {kind: 'Goto', target};
}

export function setJumpBranch(func: Func, block: number, value: ValueRef, yes: JumpTarget, no: JumpTarget): void {
  func.blocks[block].jump = {kind: 'Branch', value: unwrapRef(func, block, value), yes, no};
}

export function setJumpReturn(func: Func, block: number, value: ValueRef | null): void {
  func.blocks[block].jump = value === null ? {kind: 'ReturnVoid'} : {kind: 'Return', value: unwrapRef(func, block, value)};
}

export function setNext(func: Func, block: number, next: number): void {
  assert(next >= 0 && next < func.blocks.length);
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

export function getConstant(func: Func, ref: InsRef): number | null {
  return ref.index < 0 ? func.constants[~ref.index] : null;
}

export function getIndex(ref: InsRef): number | null {
  return ref.index >= 0 ? ref.index : null;
}

export function unwrapRef(func: Func, block: number, value: ValueRef): InsRef {
  if (value.block === block || value.block < 0) return value.ref;
  const local = spillToLocal(func, value.block, value.ref);
  return addLocalGet(func, block, local).ref;
}

function addIns(func: Func, block: number, ins: Ins): ValueRef {
  const insList = func.blocks[block].insList;
  const index = insList.length;
  insList.push(ins);
  return {block, ref: {index}};
}

export function addBinary(func: Func, block: number, op: BinIns, left: ValueRef, right: ValueRef): ValueRef {
  return addIns(func, block, {kind: 'Binary', op, left: unwrapRef(func, block, left), right: unwrapRef(func, block, right)});
}

export function addMemGet(func: Func, block: number, addr: ValueRef, offset: number, size: number): ValueRef {
  const ptr = unwrapRef(func, block, addr);
  switch (size) {
    case 1: return addIns(func, block, {kind: 'MemGet8', ptr, offset});
    case 4: return addIns(func, block, {kind: 'MemGet32', ptr, offset});
    default: throw new Error('Internal error');
  }
}

export function addMemSet(func: Func, block: number, addr: ValueRef, offset: number, size: number, ref: ValueRef): void {
  const ptr = unwrapRef(func, block, addr);
  const value = unwrapRef(func, block, ref);
  switch (size) {
    case 1: addIns(func, block, {kind: 'MemSet8', ptr, offset, value}); break;
    case 4: addIns(func, block, {kind: 'MemSet32', ptr, offset, value}); break;
    default: throw new Error('Internal error');
  }
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

export function addMemAlloc(func: Func, block: number, size: ValueRef): ValueRef {
  return addIns(func, block, {kind: 'MemAlloc', size: unwrapRef(func, block, size)});
}

export function addMemFree(func: Func, block: number, ptr: ValueRef, size: ValueRef): void {
  addIns(func, block, {kind: 'MemFree', ptr: unwrapRef(func, block, ptr), size: unwrapRef(func, block, size)});
}

export function addCall(func: Func, block: number, index: number, values: ValueRef[], retType: RawType): ValueRef {
  const args = values.map(value => unwrapRef(func, block, value));
  return addIns(func, block, {kind: 'Call', index, args, retType});
}

export function addCallImport(func: Func, block: number, index: number, values: ValueRef[], retType: RawType): ValueRef {
  const args = values.map(value => unwrapRef(func, block, value));
  return addIns(func, block, {kind: 'CallImport', index, args, retType});
}

export function addCallIntrinsic(func: Func, block: number, name: string, values: ValueRef[], retType: RawType): ValueRef {
  const args = values.map(value => unwrapRef(func, block, value));
  return addIns(func, block, {kind: 'CallIntrinsic', name, args, retType});
}

export function addPtrGlobal(func: Func, block: number, index: number): ValueRef {
  return addIns(func, block, {kind: 'PtrGlobal', index});
}

export function addRetain(func: Func, block: number, ptr: ValueRef): void {
  addIns(func, block, {kind: 'Retain', ptr: unwrapRef(func, block, ptr)});
}

export function addRelease(func: Func, block: number, ptr: ValueRef, dtor: number): void {
  addIns(func, block, {kind: 'Release', ptr: unwrapRef(func, block, ptr), dtor});
}

export function argsOf(ins: Ins): InsRef[] {
  switch (ins.kind) {
    case 'Nop': return [];

    case 'Call': return ins.args;
    case 'CallImport': return ins.args;
    case 'CallIntrinsic': return ins.args;

    case 'PtrGlobal': return [];
    case 'PtrStack': return [];

    case 'MemAlloc': return [ins.size];
    case 'MemFree': return [ins.ptr, ins.size];

    case 'MemGet8': return [ins.ptr];
    case 'MemSet8': return [ins.ptr, ins.value];
    case 'MemGet32': return [ins.ptr];
    case 'MemSet32': return [ins.ptr, ins.value];

    case 'LocalGet': return [];
    case 'LocalSet': return [ins.value];

    case 'Retain': return [ins.ptr];
    case 'Release': return [ins.ptr];

    case 'Binary': return [ins.left, ins.right];

    default: {
      const checkCovered: void = ins;
      throw new Error('Internal error');
    }
  }
}

function addUses(uses: number[], arg: InsRef): void {
  const index = getIndex(arg);
  if (index !== null) {
    assert(index >= 0 && index < uses.length);
    uses[index]++;
  }
}

export function countUses(block: BasicBlock): number[] {
  const uses: number[] = [];
  for (const ins of block.insList) {
    uses.push(0);
    for (const arg of argsOf(ins)) {
      addUses(uses, arg);
    }
  }

  switch (block.jump.kind) {
    case 'Return':
      addUses(uses, block.jump.value);
      break;

    case 'Branch':
      addUses(uses, block.jump.value);
      break;

    case 'Missing':
    case 'ReturnVoid':
    case 'Goto':
      break;

    default: {
      const checkCovered: void = block.jump;
      throw new Error('Internal error');
    }
  }

  return uses;
}

function refToString(func: Func, ref: InsRef): string {
  const constant = getConstant(func, ref);
  return constant !== null ? constant.toString() : `t${ref.index}`;
}

function blockToString(context: ToStringContext, block: BasicBlock, indent: string): string {
  let text = '';

  for (let i = 0; i < block.insList.length; i++) {
    const ins = block.insList[i];

    switch (ins.kind) {
      case 'Nop':
        break;

      case 'Call': {
        const args = ins.args.map(arg => refToString(context.func, arg));
        text += `${indent}t${i} = ${context.code.funcs[ins.index].name}(${args.join(', ')})\n`;
        break;
      }

      case 'CallImport': {
        const args = ins.args.map(arg => refToString(context.func, arg));
        text += `${indent}t${i} = ${context.code.imports[ins.index].name}(${args.join(', ')})\n`;
        break;
      }

      case 'CallIntrinsic': {
        const args = ins.args.map(arg => refToString(context.func, arg));
        text += `${indent}t${i} = ${ins.name}(${args.join(', ')})\n`;
        break;
      }

      case 'PtrGlobal':
        text += `${indent}t${i} = ${context.code.globals[ins.index].name}\n`;
        break;

      case 'PtrStack':
        text += `${indent}t${i} = s${ins.index}\n`;
        break;

      case 'MemAlloc':
        text += `${indent}t${i} = mem.alloc ${refToString(context.func, ins.size)}\n`;
        break;

      case 'MemFree':
        text += `${indent}t${i} = mem.free ${refToString(context.func, ins.ptr)}, ${refToString(context.func, ins.size)}\n`;
        break;

      case 'MemGet8':
        text += `${indent}t${i} = mem.get8 ${refToString(context.func, ins.ptr)} + ${ins.offset}\n`;
        break;

      case 'MemSet8':
        text += `${indent}t${i} = mem.set8 ${refToString(context.func, ins.ptr)} + ${ins.offset}, ${refToString(context.func, ins.value)}\n`;
        break;

      case 'MemGet32':
        text += `${indent}t${i} = mem.get32 ${refToString(context.func, ins.ptr)} + ${ins.offset}\n`;
        break;

      case 'MemSet32':
        text += `${indent}t${i} = mem.set32 ${refToString(context.func, ins.ptr)} + ${ins.offset}, ${refToString(context.func, ins.value)}\n`;
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

      case 'Binary': {
        const {op, left, right} = ins;
        switch (op) {
          case BinIns.Eq32: text += binaryToString(context, i, 'i32.eq', indent, left, right); break;
          case BinIns.NotEq32: text += binaryToString(context, i, 'i32.ne', indent, left, right); break;
          case BinIns.Lt32S: text += binaryToString(context, i, 'i32.lt_s', indent, left, right); break;
          case BinIns.Lt32U: text += binaryToString(context, i, 'i32.lt_u', indent, left, right); break;
          case BinIns.LtEq32S: text += binaryToString(context, i, 'i32.lte_s', indent, left, right); break;
          case BinIns.LtEq32U: text += binaryToString(context, i, 'i32.lte_u', indent, left, right); break;

          case BinIns.And32: text += binaryToString(context, i, 'i32.and', indent, left, right); break;
          case BinIns.Or32: text += binaryToString(context, i, 'i32.or', indent, left, right); break;
          case BinIns.Xor32: text += binaryToString(context, i, 'i32.xor', indent, left, right); break;
          case BinIns.Add32: text += binaryToString(context, i, 'i32.add', indent, left, right); break;
          case BinIns.Sub32: text += binaryToString(context, i, 'i32.sub', indent, left, right); break;
          case BinIns.Mul32: text += binaryToString(context, i, 'i32.mul', indent, left, right); break;
          case BinIns.Div32S: text += binaryToString(context, i, 'i32.div_s', indent, left, right); break;
          case BinIns.Div32U: text += binaryToString(context, i, 'i32.div_u', indent, left, right); break;
          case BinIns.Shl32: text += binaryToString(context, i, 'i32.shl', indent, left, right); break;
          case BinIns.Shr32S: text += binaryToString(context, i, 'i32.shr_s', indent, left, right); break;
          case BinIns.Shr32U: text += binaryToString(context, i, 'i32.shr_u', indent, left, right); break;

          default: {
            const checkCovered: void = op;
            throw new Error('Internal error');
          }
        }
        break;
      }

      default: {
        const checkCovered: void = ins;
        throw new Error('Internal error');
      }
    }
  }

  return text;
}

function binaryToString(context: ToStringContext, i: number, name: string, indent: string, left: InsRef, right: InsRef): string {
  return `${indent}t${i} = ${name} ${refToString(context.func, left)}, ${refToString(context.func, right)}\n`;
}

interface ToStringContext {
  code: Code;
  func: Func;
  stack: number[];
}

export function codeToString(code: Code): string {
  let text = '';

  for (const global of code.globals) {
    text += `${global.name} = ${Array.from(global.bytes).join(' ')}\n`;
  }

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
    for (let i = 0; i < func.stack.length; i++) {
      text += `  s${i} ${func.stack[i]}\n`;
    }
    text += blockTreeToString(context, 0, '  ');
  }

  return text;
}

function blockTreeToString(context: ToStringContext, index: number | null, indent: string): string {
  let text = '';

  while (index !== null) {
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

export function typeOf(func: Func, block: number, ref: InsRef): RawType {
  const target = func.blocks[block];
  const ins = target.insList[ref.index];

  switch (ins.kind) {
    case 'LocalGet':
      return func.locals[ins.local];

    case 'Nop':
    case 'MemFree':
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

    case 'Call':
    case 'CallImport':
    case 'CallIntrinsic':
      return ins.retType;

    case 'MemGet8':
    case 'MemGet32':
    case 'Binary':
      return RawType.I32;

    default: {
      const checkCovered: void = ins;
      throw new Error('Internal error');
    }
  }
}

export interface BlockMeta {
  isLive: boolean;
  isNextTarget: boolean;
  isLoopTarget: boolean;
  needsLabelAfter: boolean;
}

export function buildBlockMetas(func: Func): BlockMeta[] {
  const stack: number[] = [];
  const metas: BlockMeta[] = [];

  for (const block of func.blocks) {
    metas.push({
      isLive: false,
      isNextTarget: false,
      isLoopTarget: false,
      needsLabelAfter: false,
    });
  }

  visitBlockTree(func, metas, stack, 0);
  return metas;
}

function visitBlockTree(func: Func, metas: BlockMeta[], stack: number[], index: number): void {
  const block = func.blocks[index];
  const meta = metas[index];
  meta.isLive = true;

  stack.push(index);
  switch (block.jump.kind) {
    case 'Missing':
    case 'Return':
    case 'ReturnVoid':
      break;

    case 'Goto':
      visitJumpTarget(func, metas, stack, block.jump.target);
      break;

    case 'Branch':
      visitJumpTarget(func, metas, stack, block.jump.yes);
      visitJumpTarget(func, metas, stack, block.jump.no);
      break;

    default: {
      const checkCovered: void = block.jump;
      throw new Error('Internal error');
    }
  }
  stack.pop();

  // Only visit our sibling if we or one of our children jumps to it
  if (block.next !== null && meta.isNextTarget) {
    visitBlockTree(func, metas, stack, block.next);
  }
}

function visitJumpTarget(func: Func, metas: BlockMeta[], stack: number[], target: JumpTarget): void {
  switch (target.kind) {
    case 'Child':
      visitBlockTree(func, metas, stack, target.index);
      break;

    case 'Next': {
      const meta = metas[target.parent];
      meta.isNextTarget = true;

      // Be conservative for now (TODO: actual fallthrough detection)
      if (stack[stack.length - 1] !== target.parent) {
        meta.needsLabelAfter = true;
      }
      break;
    }

    case 'Loop': {
      const meta = metas[target.parent];
      meta.isLoopTarget = true;
      break;
    }

    default: {
      const checkCovered: void = target;
      throw new Error('Internal error');
    }
  }
}

export function hasMissingReturn(func: Func, metas: BlockMeta[]): boolean {
  for (let i = 0; i < metas.length; i++) {
    if (metas[i].isLive && func.blocks[i].jump.kind === 'Missing') {
      return true;
    }
  }
  return false;
}

export function isValidIntrinsicSignature(
  args: RawType[], retType: RawType,
  expectedArgs: RawType[], expectedRetType: RawType,
): boolean {
  if (args.length !== expectedArgs.length || retType !== expectedRetType) {
    return false;
  }

  for (let i = 0; i < args.length; i++) {
    if (args[i] !== expectedArgs[i]) {
      return false;
    }
  }

  return true;
}
