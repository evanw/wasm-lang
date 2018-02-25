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

export type Jump =
  {kind: 'Missing'} |
  {kind: 'ReturnVoid'} |
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
    previousLocals: {},
    spills: {},
  });
  return func.blocks.length - 1;
}

export function setJump(func: Func, block: number, jump: Jump): void {
  func.blocks[block].jump = jump;
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
  const ref = target.previousLocals[local] || (target.previousLocals[local] =
    addIns(func, block, {kind: 'LocalGet', local}).ref);
  return {block, ref};
}

export function addLocalSet(func: Func, block: number, local: number, value: ValueRef): ValueRef {
  const target = func.blocks[block];
  const ref = unwrapRef(func, block, value);
  target.previousLocals[local] = ref;
  return addIns(func, block, {kind: 'LocalSet', local, value: ref});
}

function refToString(func: Func, ref: InsRef): string {
  if (ref.index >= 0) return `t${ref.index}`;
  return func.constants[~ref.index].toString();
}

export function codeToString(code: Code): string {
  let text = '';

  for (const func of code.funcs) {
    const args = func.argTypes.map((arg, i) => `v${i} ${RawType[arg]}`).join(', ');
    const blocks = func.blocks;
    text += `def ${func.name}(${args}) ${RawType[func.retType]}\n`;

    text += '  locals:\n';
    for (let i = 0; i < func.locals.length; i++) {
      text += `    v${i} ${RawType[func.locals[i]]}\n`;
    }

    for (let i = 0; i < blocks.length; i++) {
      const block = blocks[i];
      text += `  b${i}:\n`;

      for (let j = 0; j < block.insList.length; j++) {
        const ins = block.insList[j];

        switch (ins.kind) {
          case 'Nop':
            break;

          case 'Alias':
            text += `    t${j} = ${refToString(func, ins.value)}\n`;
            break;

          case 'Call': {
            const args = [code.funcs[ins.index].name];
            for (const arg of ins.args) {
              args.push(refToString(func, arg));
            }
            text += `    t${j} = call ${args.join(', ')}\n`;
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
            text += `    t${j} = v${ins.local}\n`;
            break;

          case 'LocalSet':
            text += `    v${ins.local} = ${refToString(func, ins.value)}\n`;
            break;

          case 'Retain':
            text += `    retain ${refToString(func, ins.ptr)}\n`;
            break;

          case 'Release':
            text += `    release ${refToString(func, ins.ptr)}, ${code.funcs[ins.dtor].name}\n`;
            break;

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
            break;

          default: {
            const checkCovered: void = ins;
            throw new Error('Internal error');
          }
        }
      }

      switch (block.jump.kind) {
        case 'Missing':
          text += '    (missing)\n';
          break;

        case 'ReturnVoid':
          text += '    return\n';
          break;

        case 'Return':
          text += `    return ${refToString(func, block.jump.value)}\n`;
          break;

        case 'Goto':
          text += `    goto b${block.jump.target}\n`;
          break;

        case 'Branch':
          text += `    branch ${refToString(func, block.jump.value)} ? b${block.jump.yes} : b${block.jump.no}\n`;
          break;

        default: {
          const checkCovered: void = block.jump;
          throw new Error('Internal error');
        }
      }
    }
  }

  return text;
}

function spillToLocal(func: Func, block: number, ref: InsRef): number {
  assert(ref.index >= 0);
  const spills = func.blocks[block].spills;
  if (ref.index in spills) return spills[ref.index];
  const local = createLocal(func, typeOf(func, block, ref));
  addLocalSet(func, block, local, {block, ref});
  spills[ref.index] = local;
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
