import {
  BlockMeta,
  buildBlockMetas,
  Code,
  countUses,
  Func,
  getConstant,
  getIndex,
  Ins,
  InsRef,
  JumpTarget,
  RawType,
  typeOf,
  isValidIntrinsicSignature,
} from './ssa';
import { align } from './util';

declare const Buffer: any;

enum Section {
  Type = 1,
  Import = 2,
  Function = 3,
  Table = 4,
  Memory = 5,
  Global = 6,
  Export = 7,
  Start = 8,
  Element = 9,
  Code = 10,
  Data = 11,
}

enum ExternalKind {
  Function = 0,
  Table = 1,
  Memory = 2,
  Global = 3,
}

enum Type {
  I32 = -0x01,
  I64 = -0x02,
  F32 = -0x03,
  F64 = -0x04,
  AnyFunc = -0x10,
  Func = -0x20,
  Empty = -0x40,
}

enum Opcode {
  // Control flow operators
  Unreachable = 0x00,
  Nop = 0x01,
  Block = 0x02,
  Loop = 0x03,
  If = 0x04,
  Else = 0x05,
  End = 0x0B,
  Br = 0x0C,
  BrIf = 0x0D,
  BrTable = 0x0E,
  Return = 0x0F,

  // Call operators
  Call = 0x10,
  CallIndirect = 0x11,

  // Parametric operators
  Drop = 0x1A,
  Select = 0x1B,

  // Variable access
  GetLocal = 0x20,
  SetLocal = 0x21,
  TeeLocal = 0x22,
  GetGlobal = 0x23,
  SetGlobal = 0x24,

  // Memory-related operators
  I32Load = 0x28,
  I64Load = 0x29,
  F32Load = 0x2A,
  F64Load = 0x2B,
  I32Load8S = 0x2C,
  I32Load8U = 0x2D,
  I32Load16S = 0x2E,
  I32Load16U = 0x2F,
  I64Load8S = 0x30,
  I64Load8U = 0x31,
  I64Load16S = 0x32,
  I64Load16U = 0x33,
  I64Load32S = 0x34,
  I64Load32U = 0x35,
  I32Store = 0x36,
  I64Store = 0x37,
  F32Store = 0x38,
  F64Store = 0x39,
  I32Store8 = 0x3A,
  I32Store16 = 0x3B,
  I64Store8 = 0x3C,
  I64Store16 = 0x3D,
  I64Store32 = 0x3E,
  CurrentMemory = 0x3F,
  GrowMemory = 0x40,

  // Constants
  I32Const = 0x41,
  I64Const = 0x42,
  F32Const = 0x43,
  F64Const = 0x44,

  // Comparison operators
  I32Eqz = 0x45,
  I32Eq = 0x46,
  I32Ne = 0x47,
  I32LtS = 0x48,
  I32LtU = 0x49,
  I32GtS = 0x4A,
  I32GtU = 0x4B,
  I32LeS = 0x4C,
  I32LeU = 0x4D,
  I32GeS = 0x4E,
  I32GeU = 0x4F,
  I64Eqz = 0x50,
  I64Eq = 0x51,
  I64Ne = 0x52,
  I64LtS = 0x53,
  I64LtU = 0x54,
  I64GtS = 0x55,
  I64GtU = 0x56,
  I64LeS = 0x57,
  I64LeU = 0x58,
  I64GeS = 0x59,
  I64GeU = 0x5A,
  F32Eq = 0x5B,
  F32Ne = 0x5C,
  F32Lt = 0x5D,
  F32Gt = 0x5E,
  F32Le = 0x5F,
  F32Ge = 0x60,
  F64Eq = 0x61,
  F64Ne = 0x62,
  F64Lt = 0x63,
  F64Gt = 0x64,
  F64Le = 0x65,
  F64Ge = 0x66,

  // Numeric operators
  I32Clz = 0x67,
  I32Ctz = 0x68,
  I32Popcnt = 0x69,
  I32Add = 0x6A,
  I32Sub = 0x6B,
  I32Mul = 0x6C,
  I32DivS = 0x6D,
  I32DivU = 0x6E,
  I32RemS = 0x6F,
  I32RemU = 0x70,
  I32And = 0x71,
  I32Or = 0x72,
  I32Xor = 0x73,
  I32Shl = 0x74,
  I32ShrS = 0x75,
  I32ShrU = 0x76,
  I32Rotl = 0x77,
  I32Rotr = 0x78,
  I64Clz = 0x79,
  I64Ctz = 0x7A,
  I64Popcnt = 0x7B,
  I64Add = 0x7C,
  I64Sub = 0x7D,
  I64Mul = 0x7E,
  I64DivS = 0x7F,
  I64DivU = 0x80,
  I64RemS = 0x81,
  I64RemU = 0x82,
  I64And = 0x83,
  I64Or = 0x84,
  I64Xor = 0x85,
  I64Shl = 0x86,
  I64ShrS = 0x87,
  I64ShrU = 0x88,
  I64Rotl = 0x89,
  I64Rotr = 0x8A,
  F32Abs = 0x8B,
  F32Neg = 0x8C,
  F32Ceil = 0x8D,
  F32Floor = 0x8E,
  F32Trunc = 0x8F,
  F32Nearest = 0x90,
  F32Sqrt = 0x91,
  F32Add = 0x92,
  F32Sub = 0x93,
  F32Mul = 0x94,
  F32Div = 0x95,
  F32Min = 0x96,
  F32Max = 0x97,
  F32Copysign = 0x98,
  F64Abs = 0x99,
  F64Neg = 0x9A,
  F64Ceil = 0x9B,
  F64Floor = 0x9C,
  F64Trunc = 0x9D,
  F64Nearest = 0x9E,
  F64Sqrt = 0x9F,
  F64Add = 0xA0,
  F64Sub = 0xA1,
  F64Mul = 0xA2,
  F64Div = 0xA3,
  F64Min = 0xA4,
  F64Max = 0xA5,
  F64Copysign = 0xA6,

  // Conversions
  I32WrapI64 = 0xA7,
  I32TruncSF32 = 0xA8,
  I32TruncUF32 = 0xA9,
  I32TruncSF64 = 0xAA,
  I32TruncUF64 = 0xAB,
  I64ExtendSI32 = 0xAC,
  I64ExtendUI32 = 0xAD,
  I64TruncSF32 = 0xAE,
  I64TruncUF32 = 0xAF,
  I64TruncSF64 = 0xB0,
  I64TruncUF64 = 0xB1,
  F32ConvertSI32 = 0xB2,
  F32ConvertUI32 = 0xB3,
  F32ConvertSI64 = 0xB4,
  F32ConvertUI64 = 0xB5,
  F32DemoteF64 = 0xB6,
  F64ConvertSI32 = 0xB7,
  F64ConvertUI32 = 0xB8,
  F64ConvertSI64 = 0xB9,
  F64ConvertUI64 = 0xBA,
  F64PromoteF32 = 0xBB,

  // Reinterpretations
  I32ReinterpretF32 = 0xBC,
  I64ReinterpretF64 = 0xBD,
  F32ReinterpretI32 = 0xBE,
  F64ReinterpretI64 = 0xBF,
}

interface FuncType {
  argTypes: Type[];
  retType: Type | null;
}

class ByteBuffer {
  private _buffer = new Uint8Array(1024);
  private _length = 0;

  get length(): number {
    return this._length;
  }

  finish(): Uint8Array {
    return this._buffer.slice(0, this._length);
  }

  writeByte(value: number): void {
    this._growBy(1);
    this._buffer[this._length++] = value;
  }

  writeBytes(bytes: ArrayLike<number>): void {
    this._growBy(bytes.length);
    this._buffer.set(bytes, this._length);
    this._length += bytes.length;
  }

  writeVarU(value: number): void {
    do {
      let byte = value & 127;
      value = value >>> 7;
      if (value !== 0) byte = byte | 128;
      this.writeByte(byte);
    } while (value !== 0);
  }

  writeVarS(value: number): void {
    while (true) {
      let byte = value & 127;
      value = value >> 7;
      const done =
        (value == 0 && (byte & 64) === 0) ||
        (value == -1 && (byte & 64) !== 0);
      if (!done) byte = byte | 128;
      this.writeByte(byte);
      if (done) break;
    }
  }

  writeSection(id: Section, bb: ByteBuffer): void {
    this.writeVarU(id);
    this.writeVarU(bb._length);
    this.writeBytes(bb.finish());
  }

  private _growBy(bytes: number): void {
    if (this._length + bytes > this._buffer.length) {
      const buffer = new Uint8Array((this._length + bytes) * 2);
      buffer.set(this._buffer);
      this._buffer = buffer;
    }
  }
}

function rawTypeToType(type: RawType): Type {
  switch (type) {
    case RawType.I32: return Type.I32;
    case RawType.I64: return Type.I64;
    default: throw new Error('Internal error');
  }
}

function gatherTypes(code: Code): {funcTypes: FuncType[], funcIndices: number[]} {
  const funcTypeMap = new Map<string, number>();
  const funcIndices: number[] = [];
  const funcTypes: FuncType[] = [];

  for (const func of code.funcs) {
    let key = RawType[func.retType];
    for (const arg of func.argTypes) {
      key += RawType[arg];
    }

    let index = funcTypeMap.get(key);
    if (index === undefined) {
      index = funcTypes.length;
      funcTypes.push({
        argTypes: func.argTypes.map(rawTypeToType),
        retType: func.retType === RawType.Void ? null : rawTypeToType(func.retType),
      });
      funcTypeMap.set(key, index);
    }

    funcIndices.push(index);
  }

  return {funcIndices, funcTypes};
}

export function allocateGlobals(code: Code): {initializer: Uint8Array, globalOffsets: number[]} {
  const bb = new ByteBuffer;
  const globalOffsets: number[] = [];
  let offset = 1; // Reserve the byte at 0 for null pointers

  for (const global of code.globals) {
    offset = align(offset, 8);
    globalOffsets.push(offset);
    while (bb.length < offset) {
      bb.writeByte(0);
    }
    bb.writeBytes(global.bytes);
    offset += global.bytes.length;
  }

  return {globalOffsets, initializer: bb.finish()};
}

export function encodeWASM(code: Code): Uint8Array {
  const {funcTypes, funcIndices} = gatherTypes(code);
  const {initializer, globalOffsets} = allocateGlobals(code);

  // Write the WebAssembly header
  const bb = new ByteBuffer;
  bb.writeBytes([0, 0x61, 0x73, 0x6D, 1, 0, 0, 0]);

  // Write the "type" section
  const typeBB = new ByteBuffer;
  typeBB.writeVarU(funcTypes.length);
  for (const func of funcTypes) {
    typeBB.writeVarS(Type.Func);
    typeBB.writeVarU(func.argTypes.length);
    for (const arg of func.argTypes) {
      typeBB.writeVarS(arg);
    }
    if (func.retType !== null) {
      typeBB.writeVarU(1);
      typeBB.writeVarS(func.retType);
    } else {
      typeBB.writeVarU(0);
    }
  }
  bb.writeSection(Section.Type, typeBB);

  // Write the "function" section
  const functionBB = new ByteBuffer;
  functionBB.writeVarU(funcIndices.length);
  for (const index of funcIndices) {
    functionBB.writeVarU(index);
  }
  bb.writeSection(Section.Function, functionBB);

  // Write the "memory" section
  const memoryBB = new ByteBuffer;
  const initialPageCount = align(initializer.length, 1 << 16) >> 16;
  memoryBB.writeVarU(1); // 1 memory
  memoryBB.writeVarU(0); // No maximum
  memoryBB.writeVarU(initialPageCount); // Initial page count (64kb each)
  bb.writeSection(Section.Memory, memoryBB);

  // Write the "globals" section
  const globalBB = new ByteBuffer;
  globalBB.writeVarU(0); // No globals for now
  bb.writeSection(Section.Global, globalBB);

  // Write the "exports" section
  const exportBB = new ByteBuffer;
  const exported: number[] = [];
  for (let i = 0; i < code.funcs.length; i++) {
    if (!code.funcs[i].name.startsWith('_')) {
      exported.push(i);
    }
  }
  exportBB.writeVarU(exported.length);
  for (const i of exported) {
    const utf8 = new Buffer(code.funcs[i].name);
    exportBB.writeVarU(utf8.length);
    exportBB.writeBytes(utf8);
    exportBB.writeByte(ExternalKind.Function);
    exportBB.writeVarU(i);
  }
  bb.writeSection(Section.Export, exportBB);

  // Write the "code" section
  const codeBB = new ByteBuffer;
  codeBB.writeVarU(code.funcs.length);
  for (const func of code.funcs) {
    const bodyBB = new ByteBuffer;
    encodeFunc(func, globalOffsets, bodyBB);
    codeBB.writeVarU(bodyBB.length);
    codeBB.writeBytes(bodyBB.finish());
  }
  bb.writeSection(Section.Code, codeBB);

  // Write the "data" section
  const dataBB = new ByteBuffer;
  dataBB.writeVarU(1);
  dataBB.writeVarU(0);
  dataBB.writeByte(Opcode.I32Const);
  dataBB.writeVarS(0);
  dataBB.writeByte(Opcode.End);
  dataBB.writeVarU(initializer.length);
  dataBB.writeBytes(initializer);
  bb.writeSection(Section.Data, dataBB);

  return bb.finish();
}

interface OpArg {
  op: Opcode;
  arg: number | null;
}

function encodeIns(context: BlockContext, args: InsRef[], ins: Ins): void {
  const opArgs = context.opArgs;

  switch (ins.kind) {
    case 'PtrGlobal':
      opArgs.push({op: Opcode.I32Const, arg: context.globalOffsets[ins.index]});
      break;

    case 'PtrStack':
      throw new Error('Not yet implemented');

    case 'MemAlloc':
      // TODO
      opArgs.push({op: Opcode.I32Const, arg: 0});
      opArgs.push({op: Opcode.Drop, arg: null});
      opArgs.push({op: Opcode.GrowMemory, arg: 0});
      opArgs.push({op: Opcode.I32Const, arg: 1});
      opArgs.push({op: Opcode.Drop, arg: null});
      args.push(ins.size);
      break;

    case 'MemFree':
      // TODO
      opArgs.push({op: Opcode.Drop, arg: null});
      opArgs.push({op: Opcode.Drop, arg: null});
      args.push(ins.ptr, ins.size);
      break;

    case 'MemGet8':
      opArgs.push({op: Opcode.I32Load8U, arg: ins.offset});
      args.push(ins.ptr);
      break;

    case 'MemSet8':
      opArgs.push({op: Opcode.I32Store8, arg: ins.offset});
      args.push(ins.ptr, ins.value);
      break;

    case 'MemGet32':
      opArgs.push({op: Opcode.I32Load, arg: ins.offset});
      args.push(ins.ptr);
      break;

    case 'MemSet32':
      opArgs.push({op: Opcode.I32Store, arg: ins.offset});
      args.push(ins.ptr, ins.value);
      break;

    case 'Call':
      opArgs.push({op: Opcode.Call, arg: ins.index});
      args.push(...ins.args);
      break;

    case 'CallIntrinsic': {
      const argTypes: RawType[] = [];
      for (const arg of ins.args) {
        const isConstant = getConstant(context.func, arg) !== null;
        argTypes.push(isConstant ? RawType.I32 : typeOf(context.func, context.blockIndex, arg));
      }

      switch (ins.name) {
        case 'wasm.grow_memory':
          if (!isValidIntrinsicSignature(argTypes, ins.retType, [RawType.I32], RawType.I32)) {
            throw new Error(`The intrinsic "${ins.name}" is invalid`);
          }
          opArgs.push({op: Opcode.GrowMemory, arg: 0});
          break;

        case 'wasm.current_memory':
          if (!isValidIntrinsicSignature(argTypes, ins.retType, [], RawType.I32)) {
            throw new Error(`The intrinsic "${ins.name}" is invalid`);
          }
          opArgs.push({op: Opcode.CurrentMemory, arg: 0});
          break;

        case 'wasm.unreachable':
          if (!isValidIntrinsicSignature(argTypes, ins.retType, [], RawType.Void)) {
            throw new Error(`The intrinsic "${ins.name}" is invalid`);
          }
          opArgs.push({op: Opcode.Unreachable, arg: null});
          break;

        default:
          throw new Error(`Invalid WebAssembly intrinsic name "${ins.name}"`);
      }

      args.push(...ins.args);
      break;
    }

    case 'LocalGet':
      opArgs.push({op: Opcode.GetLocal, arg: ins.local});
      break;

    case 'LocalSet':
      if (opArgs.length > 0) {
        const last = opArgs[opArgs.length - 1];
        if (last.op === Opcode.GetLocal && last.arg === ins.local) {
          last.op = Opcode.TeeLocal;
          break;
        }
      }
      opArgs.push({op: Opcode.SetLocal, arg: ins.local});
      args.push(ins.value);
      break;

    case 'Retain':
    case 'Release':
      throw new Error('Not yet implemented');

    case 'Eq32': {
      const leftConst = getConstant(context.func, ins.left);
      const rightConst = getConstant(context.func, ins.right);
      if (leftConst === 0) {
        opArgs.push({op: Opcode.I32Eqz, arg: null});
        args.push(ins.right);
      } else if (rightConst === 0) {
        opArgs.push({op: Opcode.I32Eqz, arg: null});
        args.push(ins.left);
      } else {
        opArgs.push({op: Opcode.I32Eq, arg: null});
        args.push(ins.left, ins.right);
      }
      break;
    }

    case 'NotEq32': encodeBinaryIns(args, opArgs, Opcode.I32Ne, ins.left, ins.right); break;
    case 'And32': encodeBinaryIns(args, opArgs, Opcode.I32And, ins.left, ins.right); break;
    case 'Or32': encodeBinaryIns(args, opArgs, Opcode.I32Or, ins.left, ins.right); break;
    case 'Xor32': encodeBinaryIns(args, opArgs, Opcode.I32Xor, ins.left, ins.right); break;
    case 'Add32': encodeBinaryIns(args, opArgs, Opcode.I32Add, ins.left, ins.right); break;
    case 'Sub32': encodeBinaryIns(args, opArgs, Opcode.I32Sub, ins.left, ins.right); break;
    case 'Mul32': encodeBinaryIns(args, opArgs, Opcode.I32Mul, ins.left, ins.right); break;
    case 'Div32S': encodeBinaryIns(args, opArgs, Opcode.I32DivS, ins.left, ins.right); break;
    case 'Div32U': encodeBinaryIns(args, opArgs, Opcode.I32DivU, ins.left, ins.right); break;
    case 'Shl32': encodeBinaryIns(args, opArgs, Opcode.I32Shl, ins.left, ins.right); break;
    case 'Shr32S': encodeBinaryIns(args, opArgs, Opcode.I32ShrS, ins.left, ins.right); break;
    case 'Shr32U': encodeBinaryIns(args, opArgs, Opcode.I32ShrU, ins.left, ins.right); break;

    case 'Lt32S': encodeCompareIns(args, opArgs, ins.left, ins.right, Opcode.I32LtS, Opcode.I32GtS); break;
    case 'Lt32U': encodeCompareIns(args, opArgs, ins.left, ins.right, Opcode.I32LtU, Opcode.I32GtU); break;
    case 'LtEq32S': encodeCompareIns(args, opArgs, ins.left, ins.right, Opcode.I32LeS, Opcode.I32GeS); break;
    case 'LtEq32U': encodeCompareIns(args, opArgs, ins.left, ins.right, Opcode.I32LeU, Opcode.I32GeU); break;

    default: {
      const checkCovered: void = ins;
      throw new Error('Internal error');
    }
  }
}

function encodeBinaryIns(
  args: InsRef[],
  opArgs: OpArg[],
  op: Opcode,
  left: InsRef,
  right: InsRef,
): void {
  opArgs.push({op, arg: null});
  args.push(left, right);
}

function encodeCompareIns(
  args: InsRef[],
  opArgs: OpArg[],
  left: InsRef,
  right: InsRef,
  ltr: Opcode,
  rtl: Opcode,
): void {
  const leftIndex = getIndex(left);
  const rightIndex = getIndex(right);

  // Right-to-left
  if (leftIndex !== null && rightIndex !== null && leftIndex > rightIndex) {
    opArgs.push({op: rtl, arg: null});
    args.push(right, left);
  }

  // Left-to-right
  else {
    opArgs.push({op: ltr, arg: null});
    args.push(left, right);
  }
}

interface Locals {
  types: Type[];
  argCount: number;
  blockIndex: number;
  localForIns: Map<number, number>;
}

function createLocals(func: Func): Locals {
  return {
    types: func.locals.map(rawTypeToType),
    argCount: func.argTypes.length,
    blockIndex: -1,
    localForIns: new Map,
  };
}

function createTemporary(locals: Locals, blockIndex: number, insIndex: number, type: RawType): number {
  if (locals.blockIndex === blockIndex) {
    const index = locals.localForIns.get(insIndex);
    if (index !== undefined) {
      return index;
    }
  } else {
    locals.blockIndex = blockIndex;
    locals.localForIns = new Map;
  }

  const localIndex = locals.types.length;
  locals.types.push(rawTypeToType(type));
  locals.localForIns.set(insIndex, localIndex);
  return localIndex;
}

function checkForTemporary(locals: Locals, blockIndex: number, insIndex: number): number | null {
  if (locals.blockIndex === blockIndex) {
    const index = locals.localForIns.get(insIndex);
    if (index !== undefined) {
      return index;
    }
  }
  return null;
}

function finishLocals(locals: Locals, bb: ByteBuffer): number[] {
  const {argCount, types} = locals;

  // Count the number of locals in each type
  let i32Count = 0;
  let i64Count = 0;
  for (let i = locals.argCount; i < types.length; i++) {
    if (types[i] === Type.I64) i64Count++;
    else i32Count++;
  }

  // Write out the locals
  bb.writeVarU(
    (i32Count > 0 ? 1 : 0) +
    (i64Count > 0 ? 1 : 0)
  );
  if (i32Count > 0) {
    bb.writeVarU(i32Count);
    bb.writeVarS(Type.I32);
  }
  if (i64Count > 0) {
    bb.writeVarU(i64Count);
    bb.writeVarS(Type.I64);
  }

  // Remap locals into runs of the same type
  const remap: number[] = [];
  for (let i = 0; i < locals.argCount; i++) {
    remap.push(i);
  }
  let i32Next = locals.argCount;
  let i64Next = i32Count;
  for (let i = locals.argCount; i < types.length; i++) {
    remap.push(types[i] === Type.I64 ? i64Next++ : i32Next++);
  }
  return remap;
}

interface BlockContext {
  func: Func;
  globalOffsets: number[];
  locals: Locals;
  uses: number[];
  opArgs: OpArg[];
  blockIndex: number;
  insIndex: number;
}

function visitIns(context: BlockContext): void {
  const block = context.func.blocks[context.blockIndex];
  const args: InsRef[] = [];
  encodeIns(context, args, block.insList[context.insIndex]);

  // Encode the args in reverse because we're encoding from the bottom up
  for (let i = args.length - 1; i >= 0; i--) {
    visitArg(context, args[i]);
  }
}

function visitArg(context: BlockContext, arg: InsRef): void {
  const constant = getConstant(context.func, arg);

  // Is this a constant? If so, add it directly.
  if (constant !== null) {
    context.opArgs.push({op: Opcode.I32Const, arg: constant});
  }

  // If this argument is a single-use value immediately
  // before this instruction, inline that value too
  else if (context.uses[arg.index] === 1 && arg.index === context.insIndex - 1) {
    context.insIndex--;
    visitIns(context);
  }

  // Otherwise this value will have to be generated separately,
  // saved to a local, and then loaded from a local here
  else {
    const type = typeOf(context.func, context.blockIndex, {index: arg.index});
    context.opArgs.push({
      op: Opcode.GetLocal,
      arg: createTemporary(context.locals, context.blockIndex, arg.index, type),
    });
  }
}

function getInverseComparison(opcode: Opcode): Opcode | null {
  switch (opcode) {
    // 32-bit
    case Opcode.I32Eq: return Opcode.I32Ne;
    case Opcode.I32Ne: return Opcode.I32Eq;
    case Opcode.I32LtS: return Opcode.I32GeS;
    case Opcode.I32LtU: return Opcode.I32GeU;
    case Opcode.I32GtS: return Opcode.I32LeS;
    case Opcode.I32GtU: return Opcode.I32LeU;
    case Opcode.I32LeS: return Opcode.I32GtS;
    case Opcode.I32LeU: return Opcode.I32GtU;
    case Opcode.I32GeS: return Opcode.I32LtS;
    case Opcode.I32GeU: return Opcode.I32LtU;

    // 64-bit
    case Opcode.I64Eq: return Opcode.I64Ne;
    case Opcode.I64Ne: return Opcode.I64Eq;
    case Opcode.I64LtS: return Opcode.I64GeS;
    case Opcode.I64LtU: return Opcode.I64GeU;
    case Opcode.I64GtS: return Opcode.I64LeS;
    case Opcode.I64GtU: return Opcode.I64LeU;
    case Opcode.I64LeS: return Opcode.I64GtS;
    case Opcode.I64LeU: return Opcode.I64GtU;
    case Opcode.I64GeS: return Opcode.I64LtS;
    case Opcode.I64GeU: return Opcode.I64LtU;
  }

  return null;
}

function invertLastBooleanValue(stream: OpArg[]): void {
  const last = stream[stream.length - 1];

  // First try to invert the operation
  const inverse = getInverseComparison(last.op);
  if (inverse !== null) {
    last.op = inverse;
  }

  // Then try to remove the operation
  else if (last.op === Opcode.I32Eqz) {
    stream.pop();
  }

  // Otherwise, apply another invert
  else {
    stream.push({op: Opcode.I32Eqz, arg: null});
  }
}

interface LabelStackEntry {
  blockIndex: number;
  isLoop: boolean;
}

function encodeBlockTree(
  func: Func,
  globalOffsets: number[],
  locals: Locals,
  metas: BlockMeta[],
  stack: LabelStackEntry[],
  stream: OpArg[],
  blockIndex: number,
): void {
  const block = func.blocks[blockIndex];
  const opArgs: OpArg[] = [];
  const context: BlockContext = {
    func,
    globalOffsets,
    locals,
    opArgs,
    uses: countUses(block),
    blockIndex,
    insIndex: block.insList.length,
  };

  // The last instruction may be needed by the jump
  switch (block.jump.kind) {
    case 'Return':
      visitArg(context, block.jump.value);
      break;

    case 'Branch':
      visitArg(context, block.jump.value);
      break;
  }

  // Iterate over the block from bottom to top attempting to "stackify"
  // adjacent expressions into something resembling expression trees.
  // WebAssembly is encoded using a stack-based format for size reasons.
  while (context.insIndex > 0) {
    context.insIndex--;

    // If this was referenced by a later instruction, store the result of
    // this instruction in a variable so that later instruction can use it.
    const local = checkForTemporary(locals, blockIndex, context.insIndex);
    if (local !== null) {
      const lastIndex = opArgs.length - 1;
      if (lastIndex >= 0) {
        const last = opArgs[lastIndex];

        // If the immediate next instruction is going to load the value that
        // we're about to store, turn the store and load pair into a "tee"
        // since it does the same thing while using slightly less space.
        if (last.op === Opcode.GetLocal && last.arg === local) {
          last.op = Opcode.TeeLocal;
          visitIns(context);
          continue;
        }
      }
      opArgs.push({op: Opcode.SetLocal, arg: local});
    }

    visitIns(context);
    // TODO: may need a "drop" here
  }

  // Loops in WebAssembly are a special block where the label comes before the
  // block instead of afterwards.
  const meta = metas[blockIndex];
  if (meta.isLoopTarget) {
    stream.push({op: Opcode.Loop, arg: Type.Empty});
    stack.push({blockIndex, isLoop: true});
  }

  // Now we have a reversed array of WebAssembly instructions for this block.
  // Iterate over that in reverse so that they are all written out forwards.
  for (let i = opArgs.length - 1; i >= 0; i--) {
    stream.push(opArgs[i]);
  }

  switch (block.jump.kind) {
    case 'Missing':
      break;

    case 'ReturnVoid':
    case 'Return':
      stream.push({op: Opcode.Return, arg: null});
      break;

    case 'Branch': {
      let {yes, no} = block.jump;
      if (!meta.isLoopTarget) {
        stack.push({blockIndex, isLoop: false});
      }

      // If the "then" branch just falls through to the next statement, swap it
      // with the "else" branch. That way we can avoid the "else" altogether.
      if (yes.kind === 'Next' && yes.parent === blockIndex) {
        [yes, no] = [no, yes];
        invertLastBooleanValue(stream);
      }

      stream.push({op: Opcode.If, arg: Type.Empty});
      encodeJumpTarget(func, globalOffsets, locals, metas, stack, stream, yes);

      // Only create an "else" if it's not a fallthrough
      if (no.kind !== 'Next' || no.parent !== blockIndex) {
        stream.push({op: Opcode.Else, arg: null});
        encodeJumpTarget(func, globalOffsets, locals, metas, stack, stream, no);
      }

      stream.push({op: Opcode.End, arg: null});
      if (!meta.isLoopTarget) {
        stack.pop();
      }
      break;
    }

    case 'Goto':
      if (meta.needsLabelAfter) {
        stack.push({blockIndex, isLoop: false});
        stream.push({op: Opcode.Block, arg: Type.Empty});
      }
      encodeJumpTarget(func, globalOffsets, locals, metas, stack, stream, block.jump.target);
      if (meta.needsLabelAfter) {
        stream.push({op: Opcode.End, arg: null});
        stack.pop();
      }
      break;

    default: {
      const checkCovered: void = block.jump;
      throw new Error('Internal error');
    }
  }

  // End any control flow constructs we created above
  if (meta.isLoopTarget) {
    stream.push({op: Opcode.End, arg: null});
    stack.pop();
  }

  // Only encode the next block if something jumps to it
  if (meta.isNextTarget && block.next !== null) {
    encodeBlockTree(func, globalOffsets, locals, metas, stack, stream, block.next);
  }
}

function encodeJumpTarget(
  func: Func,
  globalOffsets: number[],
  locals: Locals,
  metas: BlockMeta[],
  stack: LabelStackEntry[],
  stream: OpArg[],
  target: JumpTarget,
): void {
  let parent: LabelStackEntry | null = null;

  switch (target.kind) {
    case 'Child':
      encodeBlockTree(func, globalOffsets, locals, metas, stack, stream, target.index);
      return;

    case 'Next':
      parent = {blockIndex: target.parent, isLoop: false};
      break;

    case 'Loop':
      parent = {blockIndex: target.parent, isLoop: true};
      break;

    default: {
      const checkCovered: void = target;
      throw new Error('Internal error');
    }
  }

  // Search the label stack for the parent from most recent to least recent.
  // If we don't find a label, then this must be a fallthrough jump.
  for (let i = stack.length - 1; i >= 0; i--) {
    const entry = stack[i];

    if (entry.blockIndex === parent.blockIndex && entry.isLoop === parent.isLoop) {
      stream.push({op: Opcode.Br, arg: stack.length - i - 1});
      break;
    }
  }
}

function encodeFunc(func: Func, globalOffsets: number[], bb: ByteBuffer): void {
  // Generate the instructions
  const metas = buildBlockMetas(func);
  const locals = createLocals(func);
  const stream: OpArg[] = [];
  const stack: LabelStackEntry[] = [];
  encodeBlockTree(func, globalOffsets, locals, metas, stack, stream, 0);

  // Write the function body
  const remap = finishLocals(locals, bb);
  console.log(`func ${func.name}:`);
  for (const opArg of stream) {
    bb.writeByte(opArg.op);

    if (opArg.arg !== null) {
      switch (opArg.op) {
        case Opcode.GetLocal:
        case Opcode.SetLocal:
        case Opcode.TeeLocal:
          bb.writeVarU(remap[opArg.arg]);
          console.log(`  ${Opcode[opArg.op]} ${remap[opArg.arg]}`);
          break;

        case Opcode.I32Const:
        case Opcode.I64Const:
        case Opcode.If:
        case Opcode.Loop:
        case Opcode.Block:
          bb.writeVarS(opArg.arg);
          console.log(`  ${Opcode[opArg.op]} ${opArg.arg}`);
          break;

        case Opcode.I32Load8S:
        case Opcode.I32Load8U:
        case Opcode.I64Load8S:
        case Opcode.I64Load8U:
        case Opcode.I32Store8:
        case Opcode.I64Store8:
          bb.writeVarU(0); // Align to 1 byte
          bb.writeVarU(opArg.arg);
          console.log(`  ${Opcode[opArg.op]} ${opArg.arg}`);
          break;

        case Opcode.I32Load16S:
        case Opcode.I32Load16U:
        case Opcode.I64Load16S:
        case Opcode.I64Load16U:
        case Opcode.I32Store16:
        case Opcode.I64Store16:
          bb.writeVarU(1); // Align to 2 bytes
          bb.writeVarU(opArg.arg);
          console.log(`  ${Opcode[opArg.op]} ${opArg.arg}`);
          break;

        case Opcode.I32Load:
        case Opcode.I64Load32S:
        case Opcode.I64Load32U:
        case Opcode.I32Store:
        case Opcode.I64Store32:
          bb.writeVarU(2); // Align to 4 bytes
          bb.writeVarU(opArg.arg);
          console.log(`  ${Opcode[opArg.op]} ${opArg.arg}`);
          break;

        case Opcode.I64Load:
        case Opcode.I64Store:
          bb.writeVarU(3); // Align to 8 bytes
          bb.writeVarU(opArg.arg);
          console.log(`  ${Opcode[opArg.op]} ${opArg.arg}`);
          break;

        default:
          bb.writeVarU(opArg.arg);
          console.log(`  ${Opcode[opArg.op]} ${opArg.arg}`);
          break;
      }
    } else {
      console.log(`  ${Opcode[opArg.op]}`);
    }
  }

  bb.writeVarU(Opcode.End);
}
