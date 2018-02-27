import { Code, RawType, Func } from './ssa';

declare const Buffer: any;

const FUNC_END = 0x0B;

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

interface Locals {
  groups: {count: number, type: Type, offset: number}[];
  remap: number[];
}

function gatherLocals(func: Func): Locals {
  const locals: Locals = {groups: [], remap: []};
  const infos: {type: Type, offset: number}[] = [];

  // Group locals by type
  next: for (const local of func.locals) {
    const type = rawTypeToType(local);
    for (const group of locals.groups) {
      if (group.type === type) {
        infos.push({type, offset: group.count++});
        continue next;
      }
    }
    infos.push({type, offset: 0});
    locals.groups.push({type, count: 1, offset: 0});
  }

  // Compute group offsets
  let total = 0;
  for (const group of locals.groups) {
    group.offset = total;
    total += group.count;
  }

  // Remap locals to their new ids
  for (const info of infos) {
    for (const group of locals.groups) {
      if (group.type === info.type) {
        locals.remap.push(group.offset + info.offset);
        break;
      }
    }
  }

  return locals;
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
    const {groups, remap} = gatherLocals(func);
    const bodyBB = new ByteBuffer;
    bodyBB.writeVarU(groups.length);
    for (const group of groups) {
      bodyBB.writeVarU(group.count);
      bodyBB.writeVarS(group.type);
    }
    writeBlock({code, func, bb: bodyBB}, 0);
    bodyBB.writeByte(FUNC_END);
    codeBB.writeVarU(bodyBB.length);
    codeBB.writeBytes(bodyBB.finish());
  }
  bb.writeSection(Section.Code, codeBB);

  return bb.finish();
}

interface WriteContext {
  code: Code;
  func: Func;
  bb: ByteBuffer;
}

function writeBlock(context: WriteContext, index: number): void {
  // TODO
}
