import { Code, RawType, Func, InsRef, getConstant, getIndex, countUses, typeOf } from './ssa';

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

export function encodeWASM(code: Code): Uint8Array {
  const {funcTypes, funcIndices} = gatherTypes(code);

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
  memoryBB.writeVarU(1); // 1 memory
  memoryBB.writeVarU(0); // No maximum
  memoryBB.writeVarU(0); // Initial page count (64kb each)
  bb.writeSection(Section.Memory, memoryBB);

  // Write the "globals" section
  const globalBB = new ByteBuffer;
  globalBB.writeVarU(0); // No globals for now
  bb.writeSection(Section.Global, globalBB);

  // Write the "exports" section
  const exportBB = new ByteBuffer;
  exportBB.writeVarU(code.funcs.length);
  for (let i = 0; i < code.funcs.length; i++) {
    const utf8 = new Buffer(code.funcs[i].name);
    exportBB.writeVarU(utf8.length);
    exportBB.writeBytes(utf8);
    exportBB.writeByte(ExternalKind.Function);
    exportBB.writeVarU(funcIndices[i]);
  }
  bb.writeSection(Section.Export, exportBB);

  // Write the "code" section
  const codeBB = new ByteBuffer;
  codeBB.writeVarU(code.funcs.length);
  for (const func of code.funcs) {
    const bodyBB = new ByteBuffer;
    encodeFunc(func, bodyBB);
    codeBB.writeVarU(bodyBB.length);
    codeBB.writeBytes(bodyBB.finish());
  }
  bb.writeSection(Section.Code, codeBB);

  return bb.finish();
}

function encodeFunc(func: Func, bb: ByteBuffer): void {
  interface OpArg {
    op: Opcode;
    arg: number | null;
  }

  function visitBlock(blockIndex: number): void {
    function visitIns(): void {
      const ins = block.insList[insIndex];
      const args: InsRef[] = [];

      switch (ins.kind) {
        case 'Call':
          throw new Error('Not yet implemented');

        case 'PtrGlobal':
        case 'PtrStack':
          throw new Error('Not yet implemented');

        case 'MemAlloc':
        case 'MemFree':
        case 'MemCopy':
          throw new Error('Not yet implemented');

        case 'MemGet8':
        case 'MemSet8':
        case 'MemGet32':
        case 'MemSet32':
          throw new Error('Not yet implemented');

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
          const leftConst = getConstant(func, ins.left);
          const rightConst = getConstant(func, ins.right);
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

        case 'NotEq32':
          opArgs.push({op: Opcode.I32Ne, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Lt32S':
          visitCompareIns(args, ins.left, ins.right, Opcode.I32LtS, Opcode.I32GtS);
          break;

        case 'Lt32U':
          visitCompareIns(args, ins.left, ins.right, Opcode.I32LtU, Opcode.I32GtU);
          break;

        case 'LtEq32S':
          visitCompareIns(args, ins.left, ins.right, Opcode.I32LeS, Opcode.I32GeS);
          break;

        case 'LtEq32U':
          visitCompareIns(args, ins.left, ins.right, Opcode.I32LeU, Opcode.I32GeU);
          break;

        case 'And32':
          opArgs.push({op: Opcode.I32And, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Or32':
          opArgs.push({op: Opcode.I32Or, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Xor32':
          opArgs.push({op: Opcode.I32Xor, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Add32':
          opArgs.push({op: Opcode.I32Add, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Sub32':
          opArgs.push({op: Opcode.I32Sub, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Mul32':
          opArgs.push({op: Opcode.I32Mul, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Div32S':
          opArgs.push({op: Opcode.I32DivS, arg: null});
          args.push(ins.left, ins.right);
          break;

        case 'Div32U':
          opArgs.push({op: Opcode.I32DivU, arg: null});
          args.push(ins.left, ins.right);
          break;

        default: {
          const checkCovered: void = ins;
          throw new Error('Internal error');
        }
      }

      for (let i = args.length - 1; i >= 0; i--) {
        const arg = args[i];
        const constant = getConstant(func, arg);

        // Is this a constant? If so, add it directly.
        if (constant !== null) {
          opArgs.push({op: Opcode.I32Const, arg: constant});
        }

        // If this argument is a single-use value immediately
        // before this instruction, inline that value too
        else if (uses[arg.index] === 1 && arg.index === insIndex - 1) {
          insIndex--;
          visitIns();
        }

        // Otherwise this value will have to be generated separately,
        // saved to a local, and then loaded from a local here
        else {
          opArgs.push({
            op: Opcode.GetLocal,
            arg: makeTemporary(arg.index, rawTypeToType(typeOf(func, blockIndex, {index: arg.index}))),
          });
        }
      }
    }

    function visitCompareIns(args: InsRef[], left: InsRef, right: InsRef, ltr: Opcode, rtl: Opcode): void {
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

    function makeTemporary(index: number, type: Type): number {
      const existing = insTemp.get(index);
      if (existing !== undefined) {
        return existing;
      }

      let count = tempCount.get(type);
      if (count === undefined) count = 0;

      let temps = temporaries.get(type);
      if (temps === undefined) {
        temps = [];
        temporaries.set(type, temps);
      }

      if (count < temps.length) {
        const local = temps[count];
        insTemp.set(index, local);
        return local;
      }

      const local = locals.length;
      locals.push(type);
      temps.push(local);
      insTemp.set(index, local);
      tempCount.set(type, temps.length);
      return local;
    }

    const block = func.blocks[blockIndex];
    const uses = countUses(block);
    const opArgs: OpArg[] = [];
    let insIndex = block.insList.length;

    // Maps instruction indices to the local variable for
    // that instruction (an index into "locals" below)
    const insTemp = new Map<number, number>();

    // Maps instruction types to how many temporaries of
    // that type we have used so far
    const tempCount = new Map<Type, number>();

    if (block.jump.kind === 'Return') {
      // TODO: make sure "block.jump.value" is returned
    }

    while (insIndex > 0) {
      insIndex--;

      const local = insTemp.get(insIndex);
      if (local !== undefined) {
        if (opArgs.length > 0) {
          const last = opArgs[opArgs.length - 1];
          if (last.op === Opcode.GetLocal && last.arg === local) {
            last.op = Opcode.TeeLocal;
            visitIns();
            continue;
          }
        }

        opArgs.push({op: Opcode.SetLocal, arg: local});
      }

      visitIns();
      // TODO: may need a "drop" here
    }

    blocks.push(opArgs);
  }

  // Maps instruction types to an array of newly-generated
  // local variables for that type (indices into "locals"
  // below). The array is meant to be reused across basic
  // blocks since the temporaries are local to each block.
  const temporaries = new Map<Type, number[]>();

  const locals: Type[] = func.locals.map(rawTypeToType);
  const blocks: OpArg[][] = [];
  visitBlock(0);

  // Count the number of locals in each type
  let i32Count = 0;
  let i64Count = 0;
  for (const local of locals) {
    if (local === Type.I64) i64Count++;
    else i32Count++;
  }

  // Remap locals into runs of the same type
  let i32Next = 0;
  let i64Next = i32Count;
  const remap: number[] = [];
  for (const local of locals) {
    remap.push(local === Type.I64 ? i64Next++ : i32Next++);
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

  // Generate the function body
  for (const block of blocks) {
    for (let i = block.length - 1; i >= 0; i--) {
      const opArg = block[i];
      bb.writeByte(opArg.op);
      if (opArg.arg === null) continue;

      switch (opArg.op) {
        case Opcode.GetLocal:
        case Opcode.SetLocal:
        case Opcode.TeeLocal:
          bb.writeVarU(remap[opArg.arg]);
          break;

        case Opcode.I32Const:
        case Opcode.I64Const:
          bb.writeVarS(opArg.arg);
          break;

        default:
          bb.writeVarU(opArg.arg);
          break;
      }
    }
  }

  bb.writeVarU(Opcode.End);
}
