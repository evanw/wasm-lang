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
  {kind: 'Call', index: number, args: InsRef[], type: RawType} |

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

export type Jump =
  {kind: 'Goto', target: number} |
  {kind: 'Return', value: InsRef} |
  {kind: 'Branch', value: InsRef, yes: number, no: number};

export interface BasicBlock {
  insList: Ins[];
  jump: Jump;

  // This maps the index of a local to the InsRef for this block that is known
  // to already hold the value for that local. That way we can avoid needlessly
  // reloading the same local over and over within a given block. This will be
  // updated whenever a "LocalGet" or "LocalSet" instruction is needed.
  previousLocals: {[index: number]: InsRef};

  // This maps an InsRef for this block to the index of a local variable. It's
  // used to "spill" values to a temporary local variable when they are needed
  // in another block. Adding an instruction returns a ValueRef, which contains
  // the block index. When that ValueRef is later turned into an InsRef for
  // another block via unwrapRef, a local variable is allocated if one hasn't
  // already been allocated and the variable is spilled in the source block and
  // loaded in the target block. This map tracks the spill so it can be reused
  // if it's needed again in the future instead of generating another one.
  spills: {[index: number]: number};
}

export interface Graph {
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

export interface Func {
  name: string;
  args: RawType[];
  ret: RawType;
  graph: Graph;
}

export interface Code {
  funcs: Func[];

  // This stores the byte size of each global slot (used by the "PtrGlobal" instruction)
  globals: number[];
}

export function createGraph(ptrType: RawType): Graph {
  return {
    ptrType,
    blocks: [],
    constants: [0],
    locals: [],
    stack: [],
  };
}

export function createBlock(graph: Graph): number {
  graph.blocks.push({
    insList: [],
    jump: {kind: 'Return', value: {index: ~0}},
    previousLocals: {},
    spills: {},
  });
  return graph.blocks.length - 1;
}

export interface ValueRef {
  ref: InsRef;

  // The index of the block this ref is from, will be -1 for constants
  block: number;
}

export function createLocal(graph: Graph, type: RawType): number {
  graph.locals.push(type);
  return graph.locals.length - 1;
}

export function unwrapRef(graph: Graph, block: number, value: ValueRef): InsRef {
  if (value.block === block || value.block < 0) return value.ref;
  const local = spillToLocal(graph, value.block, value.ref);
  return addLocalGet(graph, block, local).ref;
}

export function addIns(graph: Graph, block: number, ins: Ins): ValueRef {
  const insList = graph.blocks[block].insList;
  const index = insList.length;
  insList.push(ins);
  return {block, ref: {index}};
}

export function addLocalGet(graph: Graph, block: number, local: number): ValueRef {
  const target = graph.blocks[block];
  const ref = target.previousLocals[local] || (target.previousLocals[local] =
    addIns(graph, block, {kind: 'LocalGet', local}).ref);
  return {block, ref};
}

export function addLocalSet(graph: Graph, block: number, local: number, value: ValueRef): ValueRef {
  const target = graph.blocks[block];
  const ref = unwrapRef(graph, block, value);
  target.previousLocals[local] = ref;
  return addIns(graph, block, {kind: 'LocalSet', local, value: ref});
}

function spillToLocal(graph: Graph, block: number, ref: InsRef): number {
  assert(ref.index >= 0);
  const spills = graph.blocks[block].spills;
  if (ref.index in spills) return spills[ref.index];
  const local = createLocal(graph, typeOf(graph, block, ref));
  addLocalSet(graph, block, local, {block, ref});
  spills[ref.index] = local;
  return local;
}

function typeOf(graph: Graph, block: number, ref: InsRef): RawType {
  const target = graph.blocks[block];
  const ins = target.insList[ref.index];

  switch (ins.kind) {
    case 'LocalGet':
      return graph.locals[ins.local];

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
      return graph.ptrType;

    case 'Alias':
      return ins.type;

    case 'Call':
      return ins.type;

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
