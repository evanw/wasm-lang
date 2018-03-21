import { Log, Range, appendToLog } from './log';
import { Parsed, TypeExpr, CtorDecl, DefDecl, Stmt, Expr, BinOp, UnOp, Tag, Pattern } from './parser';
import {
  addLocalGet,
  addLocalSet,
  buildBlockMetas,
  Code,
  createBlock,
  createConstant,
  createFunc,
  createLocal,
  Func,
  hasMissingReturn,
  Ins,
  InsRef,
  JumpTarget,
  RawType,
  setNext,
  unwrapRef,
  ValueRef,
  addMemGet,
  addMemSet,
  createCode,
  setJumpGoto,
  setJumpBranch,
  setJumpReturn,
  addBinary,
  BinIns,
  addMemAlloc,
  addMemFree,
  addCall,
  addCallImport,
  addCallIntrinsic,
  addPtrGlobal,
  addRetain,
  addRelease,
} from './ssa';
import { assert, align } from './util';
import { optimize } from './optimize';

interface TypeID {
  index: number;
}

interface ArgData {
  typeID: TypeID;
  name: string;
  isKey: boolean;
  isRef: boolean;
}

interface CtorData {
  name: string;
  args: ArgData[];
  fieldOffsets: number[];
}

interface TypeData {
  name: string;
  ctors: CtorData[];
  dtorIndex: number;
  defaultCtor: number | null;

  // This will be false for primitive types
  hasRefCount: boolean;

  // This will be null for types with less than 2 ctors
  tagOffset: number | null;

  // The size of a field of this type
  fieldSize: number;

  // The alignment of a field of this type
  fieldAlign: number;

  // How much the allocator must reserve for this type
  allocSize: number;
}

type DefKind =
  {kind: 'Func', index: number} |
  {kind: 'Import', index: number} |
  {kind: 'Intrinsic', name: string};

interface DefData {
  name: string;
  args: ArgData[];
  retTypeID: TypeID;
  kind: DefKind;
}

type GlobalRef =
  {kind: 'Type', typeID: TypeID} |
  {kind: 'Ctor', typeID: TypeID, index: number} |
  {kind: 'Def', defID: number} |
  {kind: 'Var', varID: number};

interface VarData {
  name: string;
  typeID: TypeID;

  // The index of the entry in the "globals" table of the "Code" object
  index: number;
}

interface GlobalScope {
  // All truely global variables go here
  globals: Map<string, GlobalRef>;

  // All module-level variables (starting with "_") go here instead
  modules: Map<number, Map<string, GlobalRef>>;
}

type Temporary =
  {kind: 'Normal', typeID: TypeID, value: ValueRef} |
  {kind: 'Branch', typeID: TypeID, local: number};

interface Loop {
  block: number;
  scope: Scope;
}

interface Context {
  librarySource: number;
  code: Code;
  log: Log;
  ptrType: RawType;
  types: TypeData[];
  defs: DefData[];
  vars: VarData[];
  globalScope: GlobalScope;

  // Function-specific temporaries
  currentBlock: number;
  loops: Loop[];
  temporaries: Temporary[];

  // This holds the number of nested conditional expressions (e.g. a "?:"
  // expression) that we're currently in. If greater than 0, any temporaries
  // currently being tracked must be conditionally released, since there may be
  // control flow paths in which they were never allocated. This is done by
  // using a dedicated local for the allocation and setting it to zero before
  // the first control flow fork.
  controlFlowExprDepth: number;

  // This is the block index when the current statement was started. If local
  // variables are needed for the conditional temporary handling described
  // above, they will be zeroed here because this block dominates all uses of
  // that temporary.
  stmtStartBlockIndex: number;

  // Built-in types
  errorTypeID: TypeID;
  voidTypeID: TypeID;
  boolTypeID: TypeID;
  intTypeID: TypeID;
}

function findGlobal(scope: GlobalScope, source: number, name: string): GlobalRef | null {
  const map = scope.modules.get(source);
  if (map !== undefined) {
    const ref = map.get(name);
    if (ref !== undefined) {
      return ref;
    }
  }

  const ref = scope.globals.get(name);
  if (ref !== undefined) {
    return ref;
  }

  return null;
}

function defineGlobal(context: Context, range: Range, name: string, ref: GlobalRef): boolean {
  if (findGlobal(context.globalScope, range.source, name) !== null) {
    appendToLog(context.log, range, `The name "${name}" is already used`);
    return false;
  }

  if (!name.startsWith('_')) {
    context.globalScope.globals.set(name, ref);
    return true;
  }

  let map = context.globalScope.modules.get(range.source);
  if (map === undefined) {
    map = new Map;
    context.globalScope.modules.set(range.source, map);
  }

  map.set(name, ref);
  return true;
}

function addTypeID(context: Context, range: Range, name: string, fieldBytes: number): TypeID {
  const typeID: TypeID = {index: context.types.length};
  if (!defineGlobal(context, range, name, {kind: 'Type', typeID})) {
    return context.errorTypeID;
  }

  context.types.push({
    name,
    ctors: [],
    dtorIndex: -1,
    defaultCtor: null,
    hasRefCount: true,
    tagOffset: null,
    fieldSize: fieldBytes,
    fieldAlign: fieldBytes,
    allocSize: 1,
  });
  return typeID;
}

function beforeControlFlowFork(context: Context): void {
  context.controlFlowExprDepth += 1;
}

function afterControlFlowFork(context: Context): void {
  context.controlFlowExprDepth -= 1;
}

function trackTemporary(context: Context, func: Func, value: ValueRef, typeID: TypeID): void {
  assert(context.types[typeID.index].hasRefCount);

  // The common case of straight-line control flow is easy
  if (context.controlFlowExprDepth === 0) {
    context.temporaries.push({kind: 'Normal', typeID, value});
    return;
  }

  // Allocate a new local variable that will be used to pass this temporary
  // object to "releaseTemporaries" at the end of the statement
  const local = createLocal(func, rawTypeForTypeID(context, typeID));

  // Zero this local variable before the control flow forks (null values won't
  // be released by "releaseTemporaries")
  addLocalSet(func, context.stmtStartBlockIndex, local, createConstant(func, 0));

  // Overwrite the null value at the current instruction
  addLocalSet(func, context.currentBlock, local, value);
  context.temporaries.push({kind: 'Branch', typeID, local});
}

function releaseTemporaries(context: Context, func: Func): void {
  for (const temp of context.temporaries) {
    switch (temp.kind) {
      case 'Normal': {
        const type = context.types[temp.typeID.index];
        assert(type.hasRefCount);
        addRelease(func, context.currentBlock, temp.value, type.dtorIndex);
        break;
      }

      // Handle allocations inside conditional branches, which are stored in a
      // local variable and may be null
      case 'Branch': {
        const type = context.types[temp.typeID.index];
        assert(type.hasRefCount);
        const ptr = addLocalGet(func, context.currentBlock, temp.local);
        const release = createBlock(func);
        const next = createBlock(func);
        setJumpBranch(func, context.currentBlock, ptr,
          {kind: 'Child', index: release},
          {kind: 'Next', parent: context.currentBlock});
        addRelease(func, release, ptr, type.dtorIndex);
        setJumpGoto(func, release, {kind: 'Next', parent: context.currentBlock});
        context.currentBlock = next;
        break;
      }

      default: {
        const checkCovered: void = temp;
        throw new Error('Internal error');
      }
    }
  }

  context.temporaries = [];
}

function resolveTypeExpr(context: Context, type: TypeExpr): TypeID {
  switch (type.kind.kind) {
    case 'Void':
      return context.voidTypeID;

    case 'Name':
      return resolveTypeName(context, type.range, type.kind.name);

    default:
      appendToLog(context.log, type.range, `Unsupported type of kind "${type.kind.kind}"`);
      return context.errorTypeID;
  }
}

function resolveTypeName(context: Context, range: Range, name: string): TypeID {
  const ref = findGlobal(context.globalScope, range.source, name);

  // Check that the name exists
  if (ref === null) {
    appendToLog(context.log, range, `There is no type named "${name}"`);
    return context.errorTypeID;
  }

  // People might try to use a constructor as a type
  if (ref.kind === 'Ctor') {
    const typeName = context.types[ref.typeID.index].name;
    appendToLog(context.log, range, `Use the type "${typeName}" instead of the constructor "${name}"`);
    return context.errorTypeID;
  }

  // The name must refer to a type
  if (ref.kind !== 'Type') {
    appendToLog(context.log, range, `The name "${name}" is not a type`);
    return context.errorTypeID;
  }

  return ref.typeID;
}

function compileTypes(context: Context, parsed: Parsed): void {
  const ptrBytes = context.ptrType === RawType.I64 ? 8 : 4;
  const list: [TypeID, CtorDecl[]][] = [];

  // Prepare built-in types
  context.errorTypeID = addTypeID(context, {source: -1, start: 0, end: 0}, '(error)', 0);
  context.voidTypeID = addTypeID(context, {source: -1, start: 0, end: 0}, '(void)', 0);
  const errorType = context.types[context.errorTypeID.index];
  const voidType = context.types[context.voidTypeID.index];
  errorType.hasRefCount = voidType.hasRefCount = false;

  // Add type names first
  for (const decl of parsed.types) {
    if (decl.params.length !== 0) {
      appendToLog(context.log, decl.range, `Generic types are not currently implemented`);
      continue;
    }

    const typeID = addTypeID(context, decl.nameRange, decl.name, ptrBytes);
    if (typeID === context.errorTypeID) {
      continue;
    }
    const data = context.types[typeID.index];
    const ctors: CtorDecl[] = [];

    // Also add constructors
    for (const ctor of decl.ctors) {
      const index = data.ctors.length;

      // Allow one constructor to share the name of the type
      if (ctor.name === decl.name && data.defaultCtor === null) {
        data.defaultCtor = data.ctors.length;
        data.ctors.push({name: ctor.name, args: [], fieldOffsets: []});
        ctors.push(ctor);
      }

      // Otherwise the constructor must have a unique global name
      else if (defineGlobal(context, ctor.nameRange, ctor.name, {kind: 'Ctor', typeID, index})) {
        data.ctors.push({name: ctor.name, args: [], fieldOffsets: []});
        ctors.push(ctor);
      }
    }

    list.push([typeID, ctors]);
  }

  // Bind primitive types
  context.boolTypeID = resolveTypeName(context, {source: -1, start: 0, end: 0}, 'bool');
  context.intTypeID = resolveTypeName(context, {source: -1, start: 0, end: 0}, 'int');

  // Primitive types have a fixed size and are not reference counted
  const intType = context.types[context.intTypeID.index];
  const boolType = context.types[context.boolTypeID.index];
  intType.fieldSize = intType.fieldAlign = 4;
  boolType.fieldSize = boolType.fieldAlign = 1;
  intType.hasRefCount = boolType.hasRefCount = false;

  // Add fields next
  for (const [typeID, ctors] of list) {
    const type = context.types[typeID.index];
    let ctorOffsetStart = 0;

    // If present, the reference count comes first
    if (type.hasRefCount) {
      ctorOffsetStart += 4;
    }

    // If present, the tag comes next
    if (ctors.length > 1) {
      type.tagOffset = ctorOffsetStart;
      ctorOffsetStart += 4;
    }

    // Resolve field types now that all types have been defined
    for (let j = 0; j < ctors.length; j++) {
      const ctor = ctors[j];
      const ctorData = type.ctors[j];
      let ctorOffset = ctorOffsetStart;

      // Turn constructor arguments into fields
      for (const arg of ctor.args) {
        const typeID = resolveTypeExpr(context, arg.type);
        const fieldType = context.types[typeID.index];
        ctorData.args.push({typeID, name: arg.name, isKey: arg.isKey, isRef: arg.isRef});
        ctorOffset = align(ctorOffset, fieldType.fieldAlign);
        ctorData.fieldOffsets.push(ctorOffset);
        ctorOffset += fieldType.fieldSize;
      }

      // The size of the type is the maximum ctor size
      if (type.allocSize < ctorOffset) {
        type.allocSize = ctorOffset;
      }
    }
  }

  // Create destructors
  for (const [typeID, ctors] of list) {
    const type = context.types[typeID.index];
    if (!type.hasRefCount) {
      continue;
    }

    const func = createFunc(`dtor-${type.name}`, context.ptrType);
    const index = context.code.funcs.length;
    context.code.funcs.push(func);
    type.dtorIndex = index;

    // Set the signature
    const rawType = rawTypeForTypeID(context, typeID);
    const argIndex = createLocal(func, rawType);
    func.argTypes.push(rawType);
    func.retType = RawType.Void;

    // Decrement the reference count
    const subBlock = createBlock(func);
    const ptr = addLocalGet(func, subBlock, argIndex);
    const refCount = addMemGet(func, subBlock, ptr, 0, 4);
    const sub = addBinary(func, subBlock, BinIns.Sub32, refCount, createConstant(func, 1));

    // Update the reference count if it's greater than 0
    const updateBlock = createBlock(func);
    addMemSet(func, updateBlock, ptr, 0, 4, sub);

    // Free the object if the reference count is 0
    const freeBlock = createBlock(func);
    let currentBlock = freeBlock;

    // Release all relevant fields first
    for (let i = 0; i < type.ctors.length; i++) {
      const ctor = type.ctors[i];
      const parent = currentBlock;

      // There's no need to release fields for this constructor if there aren't any
      if (ctor.args.length === 0) {
        continue;
      }

      // Check for tag equality
      if (type.tagOffset !== null) {
        const tag = addMemGet(func, parent, ptr, type.tagOffset, 4);
        const equals = addBinary(func, parent, BinIns.Eq32, tag, createConstant(func, i));
        currentBlock = createBlock(func);
        setJumpBranch(func, parent, equals,
          {kind: 'Child', index: currentBlock},
          {kind: 'Next', parent});
      }

      // Release all fields for this constructor
      for (let j = 0; j < ctor.args.length; j++) {
        const fieldType = context.types[ctor.args[j].typeID.index];
        if (fieldType.hasRefCount) {
          const fieldPtr = addMemGet(func, currentBlock, ptr, ctor.fieldOffsets[j], fieldType.fieldSize);
          addRelease(func, currentBlock, fieldPtr, fieldType.dtorIndex);
        }
      }

      // If we checked for tag equality, merge control flow again
      if (type.tagOffset !== null) {
        setJumpGoto(func, currentBlock, {kind: 'Next', parent});
        currentBlock = createBlock(func);
        setNext(func, parent, currentBlock);
      }
    }

    // Free the memory after fields have been released
    addMemFree(func, currentBlock, ptr, createConstant(func, type.allocSize));

    // Branch off the reference count subtraction
    setJumpBranch(func, subBlock, sub,
      {kind: 'Child', index: updateBlock},
      {kind: 'Child', index: freeBlock});
  }
}

interface Local {
  typeID: TypeID;
  index: number;
}

interface Scope {
  parent: Scope | null;
  locals: Map<string, Local>;
}

function createScope(parent: Scope | null): Scope {
  return {parent, locals: new Map};
}

function getFirstTag(context: Context, tags: Tag[]): Tag | null {
  if (tags.length === 0) {
    return null;
  }

  if (tags.length > 1) {
    for (let i = 1; i < tags.length; i++) {
      appendToLog(context.log, tags[i].range, `Ignoring extra tag "${tags[i].name}"`);
    }
  }

  return tags[0];
}

function getStringArg(context: Context, tag: Tag): string | null {
  if (tag.args.length !== 1) {
    appendToLog(context.log, tag.range, `The "@${tag.name}" tag takes one argument`);
    return null;
  }

  const arg = tag.args[0];
  if (arg.kind.kind !== 'String') {
    appendToLog(context.log, arg.range, `The "@${tag.name}" argument must be a string`);
    return null;
  }

  return arg.kind.value;
}

function retainResult(context: Context, func: Func, result: Result): void {
  const type = context.types[result.typeID.index];
  if (type.hasRefCount) {
    addRetain(func, context.currentBlock, result.value);
  }
}

function compileDefs(context: Context, parsed: Parsed): void {
  const exportNames = new Set<string>();
  const list: [number, DefDecl][] = [];

  // Resolve all argument types and return types
  for (const def of parsed.defs) {
    if (def.params.length !== 0) {
      appendToLog(context.log, def.range, `Generic functions are not currently implemented`);
      continue;
    }

    const args: ArgData[] = [];
    for (const arg of def.args) {
      const typeID = resolveTypeExpr(context, arg.type);
      args.push({typeID, name: arg.name, isKey: arg.isKey, isRef: arg.isRef});
    }
    const retTypeID = resolveTypeExpr(context, def.ret);
    const tag = getFirstTag(context, def.tags);
    let intrinsicName: string | null = null;
    let importName: string | null = null;
    let exportName: string | null = null;
    let kind: DefKind;

    // Try to resolve the intrinsic name
    if (tag !== null) {
      if (tag.name === "intrinsic") {
        intrinsicName = getStringArg(context, tag);
      } else if (tag.name === "import") {
        importName = getStringArg(context, tag);
      } else if (tag.name === "export") {
        exportName = getStringArg(context, tag);

        // Check for duplicate export names
        if (exportName !== null) {
          if (exportNames.has(exportName)) {
            appendToLog(context.log, tag.range, `The export name "${exportName}" has already been used`);
            exportName = null;
          } else {
            exportNames.add(exportName);
          }
        }
      } else {
        appendToLog(context.log, tag.range, `Use of unknown tag "${tag.name}"`);
      }
    }

    // Intrinsic functions should not have a body
    if (intrinsicName !== null) {
      if (def.body !== null) {
        appendToLog(context.log, def.nameRange, `Intrinsic functions cannot be implemented`);
      }
      kind = {kind: 'Intrinsic', name: intrinsicName};
    }

    // Imported functions should also not have a body
    else if (importName !== null) {
      if (def.body !== null) {
        appendToLog(context.log, def.nameRange, `Imported functions cannot be implemented`);
      }
      const index = context.code.imports.length;
      const argTypes = args.map(arg => rawTypeForTypeID(context, arg.typeID));
      const retType = rawTypeForTypeID(context, retTypeID);
      context.code.imports.push({name: importName, argTypes, retType});
      kind = {kind: 'Import', index};
    }

    // Regular functions should have a body
    else {
      const func = createFunc(def.name, context.ptrType);
      const index = context.code.funcs.length;
      kind = {kind: 'Func', index};
      func.exportName = exportName;
      context.code.funcs.push(func);
      if (def.body === null) {
        appendToLog(context.log, def.nameRange, `Must implement "${def.name}"`);
      }

      // Remember where "malloc" and "free" are from the library
      if (def.range.source === context.librarySource) {
        if (def.name === "_malloc") {
          context.code.mallocIndex = index;
        } else if (def.name === "_free") {
          context.code.freeIndex = index;
        } else if (def.name === "_check") {
          context.code.checkIndex = index;
        }
      }
    }

    // Check for name conflicts first
    if (defineGlobal(context, def.nameRange, def.name, {kind: 'Def', defID: context.defs.length})) {
      list.push([context.defs.length, def]);
      context.defs.push({name: def.name, args, retTypeID, kind});
    }
  }

  // Compile each function that has an implementation
  for (const [defID, def] of list) {
    const data = context.defs[defID];
    if (data.kind.kind !== 'Func') {
      continue;
    }

    const func = context.code.funcs[data.kind.index];
    const scope = createScope(null);
    context.currentBlock = createBlock(func);
    func.retType = rawTypeForTypeID(context, data.retTypeID);

    // Add a local variable for each argument
    for (let i = 0; i < def.args.length; i++) {
      const arg = def.args[i];
      const typeID = data.args[i].typeID;
      const local = defineLocal(context, func, scope, arg.name, arg.nameRange, typeID);
      func.argTypes.push(rawTypeForTypeID(context, typeID));

      // Retain each argument at the top of the function
      const type = context.types[typeID.index];
      if (type.hasRefCount) {
        addRetain(func, context.currentBlock, addLocalGet(func, context.currentBlock, local));
      }
    }

    // Compile the body
    if (def.body !== null) {
      compileStmts(context, def.body, func, scope, data.retTypeID);
    }
    releaseLocalsForScope(context, func, scope);

    // Check for a missing return statement
    if (data.retTypeID !== context.voidTypeID) {
      const metas = buildBlockMetas(func);
      if (hasMissingReturn(func, metas)) {
        appendToLog(context.log, def.nameRange, `Not all code paths return a value`);
      }
    }

    // Optimize each function after compiling it
    optimize(func);
  }
}

function compileMatchOrExpr(context: Context, func: Func, scope: Scope, test: Expr, trueBlock: number): Result {
  if (test.kind.kind !== 'Match') {
    return compileExpr(context, test, func, scope, context.boolTypeID);
  }
  const {name, nameRange, args} = test.kind;

  // Check for a local first
  const local = findLocal(scope, name);
  if (local !== null) {
    appendToLog(context.log, nameRange, `The name "${name}" is not a constructor`);
    return errorResult(context, func);
  }

  // Check for a global next
  let global = findGlobal(context.globalScope, nameRange.source, name);
  if (global === null) {
    appendToLog(context.log, nameRange, `There is no symbol named "${name}" here`);
    return errorResult(context, func);
  }
  global = forwardToDefaultCtor(context, global);
  if (global.kind !== 'Ctor') {
    appendToLog(context.log, nameRange, `The name "${name}" is not a constructor ${global.kind}`);
    return errorResult(context, func);
  }

  // The type must have a tag
  const type = context.types[global.typeID.index];
  if (type.tagOffset === null) {
    appendToLog(context.log, nameRange, `Cannot match on type "${name}"`);
    return errorResult(context, func);
  }

  // Check for tag equality
  const result = compileExpr(context, test.kind.value, func, scope, global.typeID);
  const tag = addMemGet(func, context.currentBlock, result.value, type.tagOffset, 4);
  const equals = addBinary(func, context.currentBlock, BinIns.Eq32, tag, createConstant(func, global.index));

  // The match must have the same number of fields
  const ctor = type.ctors[global.index];
  if (ctor.args.length !== args.length) {
    const expected = ctor.args.length === 1 ? '' : 's';
    const found = args.length === 1 ? '' : 's';
    appendToLog(context.log, test.range, `Expected ${ctor.args.length} argument${expected} but found ${args.length} argument${found}`);
    return errorResult(context, func);
  }

  // If the tag matches, initialize local variables
  for (let i = 0; i < args.length; i++) {
    const pattern = args[i];
    const arg = ctor.args[i];
    const local = defineLocal(context, func, scope, pattern.name, pattern.range, arg.typeID);
    const type = context.types[arg.typeID.index];
    const value = addMemGet(func, trueBlock, result.value, ctor.fieldOffsets[i], type.fieldSize);

    // Retain the value before we store it
    if (type.hasRefCount) {
      addRetain(func, trueBlock, value);
    }

    addLocalSet(func, trueBlock, local, value);
  }

  return {
    typeID: context.boolTypeID,
    value: equals,
  };
}

function releaseLocalsForScope(context: Context, func: Func, scope: Scope): void {
  var locals = scope.locals;

  for (const local of locals.values()) {
    const type = context.types[local.typeID.index];
    if (type.hasRefCount) {
      const ptr = addLocalGet(func, context.currentBlock, local.index);
      addRelease(func, context.currentBlock, ptr, type.dtorIndex);
    }
  }
}

function releaseLocalsForScopesUpTo(context: Context, func: Func, scope: Scope | null, upTo: Scope | null): void {
  while (scope !== null && scope !== upTo) {
    releaseLocalsForScope(context, func, scope);
    scope = scope.parent;
  }
}

function compileStmts(context: Context, stmts: Stmt[], func: Func, parent: Scope, retTypeID: TypeID): void {
  const scope = createScope(parent);

  for (const stmt of stmts) {
    context.stmtStartBlockIndex = context.currentBlock;

    switch (stmt.kind.kind) {
      case 'Var': {
        const type = stmt.kind.type;
        const value = stmt.kind.value;
        let typeID: TypeID;
        let initial: Result;

        // Inferred type
        if (type.kind.kind === 'Inferred') {
          initial = compileExpr(context, value, func, scope, null);
          typeID = initial.typeID;

          // Forbid creating variables of certain types
          if (typeID === context.voidTypeID) {
            appendToLog(context.log, stmt.kind.nameRange, `Cannot create a variable of type "${context.types[typeID.index].name}"`);
            typeID = context.errorTypeID;
          }
        }

        // Explicit type
        else {
          typeID = resolveTypeExpr(context, type);
          initial = compileExpr(context, value, func, scope, typeID);
        }

        const local = defineLocal(context, func, scope, stmt.kind.name, stmt.kind.nameRange, typeID);
        retainResult(context, func, initial);
        addLocalSet(func, context.currentBlock, local, initial.value);
        releaseTemporaries(context, func);
        break;
      }

      case 'Return': {
        // Return a value
        if (stmt.kind.value !== null) {
          if (retTypeID === context.voidTypeID) {
            appendToLog(context.log, stmt.kind.value.range, `Unexpected return value in a function without a return type`);
          } else {
            const result = compileExpr(context, stmt.kind.value, func, scope, retTypeID);
            retainResult(context, func, result);
            releaseTemporaries(context, func);
            releaseLocalsForScopesUpTo(context, func, scope, null);
            setJumpReturn(func, context.currentBlock, result.value);
          }
        }

        // Return nothing
        else {
          if (retTypeID !== context.voidTypeID) {
            appendToLog(context.log, stmt.range, `Must return a value of type "${context.types[retTypeID.index].name}"`);
          } else {
            releaseTemporaries(context, func);
            releaseLocalsForScopesUpTo(context, func, scope, null);
            setJumpReturn(func, context.currentBlock, null);
          }
        }

        // Add any dead code following this statement to an unused block
        context.currentBlock = createBlock(func);
        break;
      }

      case 'Break': {
        const count = stmt.kind.count;

        // The parser handles out-of-bounds error reporting
        if (count >= 1 && count <= context.loops.length) {
          const loop = context.loops[context.loops.length - count];
          releaseLocalsForScopesUpTo(context, func, scope, loop.scope);
          setJumpGoto(func, context.currentBlock, {kind: 'Next', parent: loop.block});

          // Add any dead code following this statement to an unused block
          context.currentBlock = createBlock(func);
        }
        break;
      }

      case 'Continue': {
        const count = stmt.kind.count;

        // The parser handles out-of-bounds error reporting
        if (count >= 1 && count <= context.loops.length) {
          const loop = context.loops[context.loops.length - count];
          releaseLocalsForScopesUpTo(context, func, scope, loop.scope);
          setJumpGoto(func, context.currentBlock, {kind: 'Loop', parent: loop.block});

          // Add any dead code following this statement to an unused block
          context.currentBlock = createBlock(func);
        }
        break;
      }

      case 'If': {
        const thenBlock = createBlock(func);
        const thenScope = createScope(scope);
        const test = compileMatchOrExpr(context, func, thenScope, stmt.kind.test, thenBlock);
        const hasMatchLocals = thenScope.locals.size !== 0;
        const parent = context.currentBlock;

        // We can only release the locals now if this wasn't a match expression
        if (!hasMatchLocals) {
          releaseTemporaries(context, func);
        }

        // Without an else
        if (!hasMatchLocals && stmt.kind.no.length === 0) {
          setJumpBranch(func, parent, test.value,
            {kind: 'Child', index: thenBlock},
            {kind: 'Next', parent});

          // Then branch
          context.currentBlock = thenBlock;
          compileStmts(context, stmt.kind.yes, func, thenScope, retTypeID);
          setJumpGoto(func, context.currentBlock, {kind: 'Next', parent});
        }

        // With an else
        else {
          const elseBlock = createBlock(func);
          setJumpBranch(func, parent, test.value,
            {kind: 'Child', index: thenBlock},
            {kind: 'Child', index: elseBlock});

          // Then branch
          const temporariesForElse = context.temporaries;
          context.currentBlock = thenBlock;
          releaseTemporaries(context, func);
          compileStmts(context, stmt.kind.yes, func, thenScope, retTypeID);
          releaseLocalsForScope(context, func, thenScope);
          setJumpGoto(func, context.currentBlock, {kind: 'Next', parent});

          // Else branch
          context.currentBlock = elseBlock;
          context.temporaries = temporariesForElse;
          releaseTemporaries(context, func); // Release temporaries again for the else branch
          compileStmts(context, stmt.kind.no, func, scope, retTypeID);
          setJumpGoto(func, context.currentBlock, {kind: 'Next', parent});
        }

        // Merge the control flow for following statements
        const next = createBlock(func);
        setNext(func, parent, next);
        context.currentBlock = next;
        break;
      }

      case 'While': {
        const previous = context.currentBlock;
        const loop = createBlock(func);

        // Create the loop header
        const header = createBlock(func);
        setJumpGoto(func, previous, {kind: 'Next', parent: previous});
        setNext(func, previous, loop);
        setJumpGoto(func, loop, {kind: 'Child', index: header});
        context.currentBlock = header;

        // Compile the test (special-case "while true" for "return" statement checking)
        const testExpr = stmt.kind.test;
        const body = createBlock(func);
        const bodyScope = createScope(scope);
        if (testExpr.kind.kind === 'Bool' && testExpr.kind.value === true) {
          setJumpGoto(func, header, {kind: 'Next', parent: header});
          setNext(func, context.currentBlock, body);
          context.currentBlock = body;
        } else {
          const test = compileMatchOrExpr(context, func, bodyScope, stmt.kind.test, body);
          const hasMatchLocals = bodyScope.locals.size !== 0;

          // We can only release the locals now if this wasn't a match expression
          if (!hasMatchLocals) {
            releaseTemporaries(context, func);
            setJumpBranch(func, context.currentBlock, test.value,
              {kind: 'Next', parent: context.currentBlock},
              {kind: 'Next', parent: loop});
            setNext(func, context.currentBlock, body);
            context.currentBlock = body;
          }

          // Match expressions must release locals in both branches
          else {
            const temporariesForBody = context.temporaries;
            const elseBlock = createBlock(func);
            setJumpBranch(func, context.currentBlock, test.value,
              {kind: 'Next', parent: context.currentBlock},
              {kind: 'Child', index: elseBlock});
            setNext(func, context.currentBlock, body);

            // Release temporaries before breaking out of the loop
            context.currentBlock = elseBlock;
            releaseTemporaries(context, func);
            setJumpGoto(func, elseBlock, {kind: 'Next', parent: loop});

            // Release temporaries again before continuing the loop
            context.currentBlock = body;
            context.temporaries = temporariesForBody;
            releaseTemporaries(context, func);
          }
        }

        // Compile the body
        context.loops.push({scope, block: loop});
        compileStmts(context, stmt.kind.body, func, bodyScope, retTypeID);
        releaseLocalsForScope(context, func, bodyScope);
        context.loops.pop();
        setJumpGoto(func, context.currentBlock, {kind: 'Loop', parent: loop});

        // Merge the control flow for following statements
        const next = createBlock(func);
        setNext(func, loop, next);
        context.currentBlock = next;
        break;
      }

      case 'Assign': {
        const target = stmt.kind.target;
        if (target.kind.kind !== 'Name') {
          appendToLog(context.log, target.range, `Invalid assignment target`);
          break;
        }
        const name = target.kind.value;

        // Check for a local first
        const local = findLocal(scope, name);
        if (local !== null) {
          const result = compileExpr(context, stmt.kind.value, func, scope, local.typeID);
          const type = context.types[local.typeID.index];

          // Balance reference counts
          if (type.hasRefCount) {
            addRetain(func, context.currentBlock, result.value);
            addRelease(func, context.currentBlock, addLocalGet(func, context.currentBlock, local.index), type.dtorIndex);
          }

          addLocalSet(func, context.currentBlock, local.index, result.value);
          releaseTemporaries(context, func);
          break;
        }

        // Check for a global next
        const global = findGlobal(context.globalScope, stmt.range.source, name);
        if (global !== null) {
          if (global.kind === 'Var') {
            const data = context.vars[global.varID];
            const ptr = addPtrGlobal(func, context.currentBlock, data.index);
            const size = context.types[data.typeID.index].fieldSize;
            const result = compileExpr(context, stmt.kind.value, func, scope, data.typeID);
            addMemSet(func, context.currentBlock, ptr, 0, size, result.value);
            releaseTemporaries(context, func);
          } else {
            appendToLog(context.log, target.range, `Cannot use the name "${name}" as a value`);
          }
          break;
        }

        appendToLog(context.log, target.range, `There is no variable named "${name}" here`);
        break;
      }

      case 'Expr':
        compileExpr(context, stmt.kind.value, func, scope, null);
        releaseTemporaries(context, func);
        break;

      default: {
        const checkCovered: void = stmt.kind;
        throw new Error('Internal error');
      }
    }
  }

  releaseLocalsForScope(context, func, scope);
}

interface Result {
  typeID: TypeID;
  value: ValueRef;
}

function errorResult(context: Context, func: Func): Result {
  return {typeID: context.errorTypeID, value: createConstant(func, 0)};
}

function compileArgs(context: Context, range: Range, exprs: Expr[], func: Func, scope: Scope, args: ArgData[]): Result[] {
  const results: Result[] = [];

  if (exprs.length !== args.length) {
    const expected = args.length === 1 ? '' : 's';
    const found = exprs.length === 1 ? '' : 's';
    appendToLog(context.log, range, `Expected ${args.length} argument${expected} but found ${exprs.length} argument${found}`);

    for (let i = 0; i < args.length; i++) {
      results.push(errorResult(context, func));
    }

    return results;
  }

  for (let i = 0; i < args.length; i++) {
    let expr = exprs[i];
    const arg = args[i];

    // Check for an expected key
    if (arg.isKey) {
      if (expr.kind.kind !== 'Key') {
        // Allow omitting the keyword for a name expression with the same name
        if (expr.kind.kind !== 'Name' || expr.kind.value !== arg.name) {
          appendToLog(context.log, expr.range, `Expected the keyword "${arg.name}:" before this argument`);
        }
      } else if (expr.kind.name !== arg.name) {
        appendToLog(context.log, expr.kind.nameRange, `Expected "${arg.name}:" but found "${expr.kind.name}:"`);
      }
    }

    // Check for an unexpected key
    else if (expr.kind.kind === 'Key') {
      if (expr.kind.name === arg.name) {
        appendToLog(context.log, expr.kind.nameRange, `The argument "${arg.name}" is not a keyword argument`);
      } else {
        appendToLog(context.log, expr.kind.nameRange, `Unexpected keyword "${expr.kind.name}:" before this argument`);
      }
    }

    // Unwrap the key if there is one
    if (expr.kind.kind === 'Key') {
      expr = expr.kind.value;
    }

    // Check for an expected ref
    if (arg.isRef) {
      if (expr.kind.kind !== 'Ref') {
        appendToLog(context.log, expr.range, `Expected "&" before this argument`);
      } else {
        const value = expr.kind.value;
        if (value.kind.kind !== 'Name' || findLocal(scope, value.kind.value) === null) {
          appendToLog(context.log, expr.range, `The "&" operator only works on local variable names`);
        }
      }
    }

    // Check for an unexpected ref
    else if (expr.kind.kind === 'Ref') {
      appendToLog(context.log, expr.range, `Cannot use "&" with this argument`);
    }

    // Unwrap the ref if there is one
    if (expr.kind.kind === 'Ref') {
      expr = expr.kind.value;
    }

    results.push(compileExpr(context, expr, func, scope, arg.typeID));
  }

  return results;
}

function allocType(context: Context, func: Func, typeID: TypeID, ctorIndex: number, args: Result[]): ValueRef {
  const type = context.types[typeID.index];
  const ptr = addMemAlloc(func, context.currentBlock, createConstant(func, type.allocSize));

  // Initialize the reference count to 1
  if (type.hasRefCount) {
    addMemSet(func, context.currentBlock, ptr, 0, 4, createConstant(func, 1));
  }

  // Tag the allocation with the constructor that initialized it
  if (type.tagOffset !== null) {
    addMemSet(func, context.currentBlock, ptr, type.tagOffset, 4, createConstant(func, ctorIndex));
  }

  // Initialize each field
  const ctor = type.ctors[ctorIndex];
  for (let i = 0; i < args.length; i++) {
    const fieldType = context.types[ctor.args[i].typeID.index];
    const size = fieldType.fieldSize;
    const arg = args[i];
    retainResult(context, func, arg);
    addMemSet(func, context.currentBlock, ptr, ctor.fieldOffsets[i], size, arg.value);
  }

  // Release this object at the end of the statement if it wasn't stored anywhere
  trackTemporary(context, func, ptr, typeID);
  return ptr;
}

function compileExpr(context: Context, expr: Expr, func: Func, scope: Scope, castTo: TypeID | null): Result {
  let result = errorResult(context, func);

  switch (expr.kind.kind) {
    case 'Bool':
      result = {
        typeID: context.boolTypeID,
        value: createConstant(func, expr.kind.value ? 1 : 0),
      };
      break;

    case 'Int':
      result = {
        typeID: context.intTypeID,
        value: createConstant(func, expr.kind.value),
      };
      break;

    case 'String':
      appendToLog(context.log, expr.range, `String literals are not currently implemented`);
      break;

    case 'Name': {
      const name = expr.kind.value;

      // Check for a local first
      const local = findLocal(scope, name);
      if (local !== null) {
        result = {
          typeID: local.typeID,
          value: addLocalGet(func, context.currentBlock, local.index),
        };
        break;
      }

      // Check for a global next
      let global = findGlobal(context.globalScope, expr.range.source, name);
      if (global !== null) {
        global = forwardToDefaultCtor(context, global);
        if (global.kind === 'Ctor') {
          const type = context.types[global.typeID.index];
          const ctor = type.ctors[global.index];
          const args = compileArgs(context, expr.range, [], func, scope, ctor.args);
          result = {
            typeID: global.typeID,
            value: allocType(context, func, global.typeID, global.index, args),
          };
        } else if (global.kind === 'Var') {
          const data = context.vars[global.varID];
          const ptr = addPtrGlobal(func, context.currentBlock, data.index);
          const size = context.types[data.typeID.index].fieldSize;
          result = {
            typeID: data.typeID,
            value: addMemGet(func, context.currentBlock, ptr, 0, size),
          };
        } else {
          appendToLog(context.log, expr.range, `Cannot use the name "${name}" as a value`);
        }
        break;
      }

      appendToLog(context.log, expr.range, `There is no symbol named "${name}" here`);
      break;
    }

    case 'Array':
      appendToLog(context.log, expr.range, `Array literals are not currently implemented`);
      break;

    case 'Unary':
      switch (expr.kind.op) {
        case UnOp.Cpl: {
          const operand = compileExpr(context, expr.kind.value, func, scope, context.intTypeID);
          result = {
            typeID: context.intTypeID,
            value: addBinary(func, context.currentBlock, BinIns.Xor32, operand.value, createConstant(func, -1)),
          };
          break;
        }

        case UnOp.Neg: {
          const operand = compileExpr(context, expr.kind.value, func, scope, context.intTypeID);
          result = {
            typeID: context.intTypeID,
            value: addBinary(func, context.currentBlock, BinIns.Sub32, createConstant(func, 0), operand.value),
          };
          break;
        }

        case UnOp.Not: {
          const operand = compileExpr(context, expr.kind.value, func, scope, context.boolTypeID);
          result = {
            typeID: context.boolTypeID,
            value: addBinary(func, context.currentBlock, BinIns.Eq32, operand.value, createConstant(func, 0)),
          };
          break;
        }

        default: {
          const checkCovered: void = expr.kind.op;
          throw new Error('Internal error');
        }
      }
      break;

    case 'Binary':
      switch (expr.kind.op) {
        case BinOp.BitAnd:
        case BinOp.BitOr:
        case BinOp.BitXor:
        case BinOp.Add:
        case BinOp.Sub:
        case BinOp.Mul:
        case BinOp.Div:
        case BinOp.Shl:
        case BinOp.Shr: {
          result = compileMath32(context, func, scope, expr.kind.op, expr.kind.left, expr.kind.right);
          break;
        }

        case BinOp.Eq:
        case BinOp.NotEq: {
          result = compileEqual(context, func, scope, expr.kind.op, expr.kind.left, expr.kind.right);
          break;
        }

        case BinOp.Lt:
        case BinOp.LtEq:
        case BinOp.Gt:
        case BinOp.GtEq: {
          result = compileCompare32(context, func, scope, expr.kind.op, expr.kind.left, expr.kind.right);
          break;
        }

        case BinOp.And:
        case BinOp.Or: {
          result = compileShortCircuit(context, func, scope, expr.kind.op, expr.kind.left, expr.kind.right);
          break;
        }

        default: {
          const checkCovered: void = expr.kind.op;
          throw new Error('Internal error');
        }
      }
      break;

    case 'Call': {
      const name = expr.kind.name;

      // Check for a local first
      if (findLocal(scope, name) !== null) {
        appendToLog(context.log, expr.kind.nameRange, `Cannot call local variable "${name}"`);
        break;
      }

      // Check for a global next
      let global = findGlobal(context.globalScope, expr.range.source, name);
      if (global === null) {
        appendToLog(context.log, expr.kind.nameRange, `There is no symbol named "${name}" here`);
        break;
      }

      // Check for a constructor
      global = forwardToDefaultCtor(context, global);
      if (global.kind === 'Ctor') {
        const type = context.types[global.typeID.index];
        const ctor = type.ctors[global.index];
        const args = compileArgs(context, expr.range, expr.kind.args, func, scope, ctor.args);
        result = {
          typeID: global.typeID,
          value: allocType(context, func, global.typeID, global.index, args),
        };
      }

      // Check for a function
      else if (global.kind === 'Def') {
        const def = context.defs[global.defID];
        const retType = rawTypeForTypeID(context, def.retTypeID);
        const results = compileArgs(context, expr.range, expr.kind.args, func, scope, def.args);
        const args = results.map(result => result.value);
        switch (def.kind.kind) {
          case 'Func':
            result = {
              typeID: def.retTypeID,
              value: addCall(func, context.currentBlock, def.kind.index, args, retType),
            };
            break;

          case 'Import':
            result = {
              typeID: def.retTypeID,
              value: addCallImport(func, context.currentBlock, def.kind.index, args, retType),
            };
            break;

          case 'Intrinsic':
            result = {
              typeID: def.retTypeID,
              value: addCallIntrinsic(func, context.currentBlock, def.kind.name, args, retType),
            };
            break;

          default: {
            const checkCovered: void = def.kind;
            throw new Error('Internal error');
          }
        }

        // We own a reference count on the returned object, so make sure it's released
        const type = context.types[result.typeID.index];
        if (type.hasRefCount) {
          trackTemporary(context, func, result.value, result.typeID);
        }
      }

      else {
        appendToLog(context.log, expr.range, `Cannot use "${name}" as a value here`);
      }
      break;
    }

    case 'Dot': {
      const name = expr.kind.name;
      const target = compileExpr(context, expr.kind.target, func, scope, null);
      if (target.typeID !== context.errorTypeID) {
        const type = context.types[target.typeID.index];
        if (type.ctors.length !== 1) {
          appendToLog(context.log, expr.kind.nameRange, `Cannot access the field "${name}" because the type "${type.name}" has multiple constructors`);
        } else {
          const ctorData = type.ctors[0];
          let found = false;
          for (let i = 0; i < ctorData.args.length; i++) {
            const arg = ctorData.args[i];
            if (arg.name === name) {
              found = true;
              result = {
                typeID: arg.typeID,
                value: addMemGet(func, context.currentBlock, target.value,
                  ctorData.fieldOffsets[i], context.types[arg.typeID.index].fieldSize),
              };
              break;
            }
          }
          if (!found) {
            appendToLog(context.log, expr.kind.nameRange, `There is no field named "${name}" on type "${type.name}"`);
          }
        }
      }
      break;
    }

    case 'Branch': {
      const test = compileExpr(context, expr.kind.test, func, scope, context.boolTypeID);
      const parent = context.currentBlock;
      beforeControlFlowFork(context);

      // Then branch
      const thenBlock = createBlock(func);
      context.currentBlock = thenBlock;
      const yes = compileExpr(context, expr.kind.yes, func, scope, null);
      setJumpGoto(func, thenBlock, {kind: 'Next', parent});

      // Else branch
      const elseBlock = createBlock(func);
      context.currentBlock = elseBlock;
      const no = compileExpr(context, expr.kind.no, func, scope, null);
      setJumpGoto(func, elseBlock, {kind: 'Next', parent});

      // Merge the control flow for following expressions
      setJumpBranch(func, parent, test.value,
        {kind: 'Child', index: thenBlock},
        {kind: 'Child', index: elseBlock});
      const next = createBlock(func);
      setNext(func, parent, next);
      context.currentBlock = next;
      afterControlFlowFork(context);

      // Merge the data using a local variable
      if (yes.typeID !== no.typeID) {
        appendToLog(context.log, expr.range, `Expected branch types "${context.types[yes.typeID.index].name}" and "${context.types[no.typeID.index].name}" to be the same`);
      } else {
        const local = createLocal(func, rawTypeForTypeID(context, yes.typeID));
        addLocalSet(func, thenBlock, local, yes.value);
        addLocalSet(func, elseBlock, local, no.value);
        result = {
          typeID: yes.typeID,
          value: addLocalGet(func, next, local),
        };
      }
      break;
    }

    case 'Index':
      appendToLog(context.log, expr.range, `The "[]" operator is not currently implemented`);
      break;

    case 'Ref':
      appendToLog(context.log, expr.range, `Cannot use "&" here`);
      break;

    case 'Key':
    case 'Match':
      throw new Error('Internal error');

    default: {
      const checkCovered: void = expr.kind;
      throw new Error('Internal error');
    }
  }

  if (castTo === null) {
    return result;
  }

  return cast(context, expr.range, result, castTo, func);
}

function compileEqual(context: Context, func: Func, scope: Scope, op: BinOp, leftExpr: Expr, rightExpr: Expr): Result {
  const l = compileExpr(context, leftExpr, func, scope, null);
  const r = compileExpr(context, rightExpr, func, scope, null);

  if (l.typeID === context.errorTypeID || r.typeID === context.errorTypeID) {
    return {
      typeID: context.boolTypeID,
      value: createConstant(func, 0),
    };
  }

  if (l.typeID !== r.typeID) {
    appendToLog(context.log, {source: leftExpr.range.source, start: leftExpr.range.start, end: rightExpr.range.end},
      `Cannot compare type "${context.types[l.typeID.index].name}" and type "${context.types[r.typeID.index].name}"`);
    return {
      typeID: context.boolTypeID,
      value: createConstant(func, 0),
    };
  }

  let ins: BinIns;

  switch (op) {
    case BinOp.Eq: ins = BinIns.Eq32; break;
    case BinOp.NotEq: ins = BinIns.NotEq32; break;
    default: throw new Error('Internal error');
  }

  return {
    typeID: context.boolTypeID,
    value: addBinary(func, context.currentBlock, ins, l.value, r.value),
  };
}

function compileCompare32(context: Context, func: Func, scope: Scope, op: BinOp, leftExpr: Expr, rightExpr: Expr): Result {
  const left = compileExpr(context, leftExpr, func, scope, context.intTypeID).value;
  const right = compileExpr(context, rightExpr, func, scope, context.intTypeID).value;
  let value: ValueRef;

  switch (op) {
    case BinOp.Lt: value = addBinary(func, context.currentBlock, BinIns.Lt32S, left, right); break;
    case BinOp.LtEq: value = addBinary(func, context.currentBlock, BinIns.LtEq32S, left, right); break;
    case BinOp.Gt: value = addBinary(func, context.currentBlock, BinIns.Lt32S, right, left); break;
    case BinOp.GtEq: value = addBinary(func, context.currentBlock, BinIns.LtEq32S, right, left); break;
    default: throw new Error('Internal error');
  }

  return {typeID: context.boolTypeID, value};
}

function compileMath32(context: Context, func: Func, scope: Scope, op: BinOp, leftExpr: Expr, rightExpr: Expr): Result {
  let ins: BinIns;

  switch (op) {
    case BinOp.BitOr: ins = BinIns.Or32; break;
    case BinOp.BitXor: ins = BinIns.Xor32; break;
    case BinOp.BitAnd: ins = BinIns.And32; break;
    case BinOp.Add: ins = BinIns.Add32; break;
    case BinOp.Sub: ins = BinIns.Sub32; break;
    case BinOp.Mul: ins = BinIns.Mul32; break;
    case BinOp.Div: ins = BinIns.Div32S; break;
    case BinOp.Shl: ins = BinIns.Shl32; break;
    case BinOp.Shr: ins = BinIns.Shr32S; break;
    default: throw new Error('Internal error');
  }

  return {
    typeID: context.intTypeID,
    value: addBinary(func, context.currentBlock, ins,
      compileExpr(context, leftExpr, func, scope, context.intTypeID).value,
      compileExpr(context, rightExpr, func, scope, context.intTypeID).value),
  };
}

function compileShortCircuit(context: Context, func: Func, scope: Scope, op: BinOp, leftExpr: Expr, rightExpr: Expr): Result {
  // Compile the left
  const local = createLocal(func, RawType.I32);
  const left = compileExpr(context, leftExpr, func, scope, context.boolTypeID);
  const leftEnd = context.currentBlock;
  addLocalSet(func, leftEnd, local, left.value);
  beforeControlFlowFork(context);

  // Skip the right if the condition was met
  const rightStart = createBlock(func);
  const next = createBlock(func);
  setJumpBranch(func, leftEnd, left.value,
    op === BinOp.And ? {kind: 'Child', index: rightStart} : {kind: 'Next', parent: leftEnd},
    op === BinOp.And ? {kind: 'Next', parent: leftEnd} : {kind: 'Child', index: rightStart});
  setNext(func, leftEnd, next);

  // Otherwise evaluate the right
  context.currentBlock = rightStart;
  const right = compileExpr(context, rightExpr, func, scope, context.boolTypeID);
  const rightEnd = context.currentBlock;
  addLocalSet(func, rightEnd, local, right.value);
  setJumpGoto(func, rightEnd, {kind: 'Next', parent: leftEnd});

  // Merge the control flow for following expressions
  context.currentBlock = next;
  afterControlFlowFork(context);
  return {
    typeID: context.boolTypeID,
    value: addLocalGet(func, context.currentBlock, local),
  };
}

function forwardToDefaultCtor(context: Context, global: GlobalRef): GlobalRef {
  if (global.kind === 'Type') {
    const type = context.types[global.typeID.index];
    if (type.defaultCtor !== null) {
      return {kind: 'Ctor', typeID: global.typeID, index: type.defaultCtor};
    }
  }
  return global;
}

function cast(context: Context, range: Range, result: Result, to: TypeID, func: Func): Result {
  if (result.typeID === context.errorTypeID || to === context.errorTypeID) {
    return errorResult(context, func);
  }

  if (result.typeID !== to) {
    const expected = context.types[to.index].name;
    const found = context.types[result.typeID.index].name;
    appendToLog(context.log, range, `Expected type "${expected}" but found type "${found}"`);
    return errorResult(context, func);
  }

  return result;
}

function findLocal(scope: Scope, name: string): Local | null {
  const local = scope.locals.get(name);
  if (local !== undefined) {
    return local;
  }

  if (scope.parent !== null) {
    return findLocal(scope.parent, name);
  }

  return null;
}

function rawTypeForTypeID(context: Context, typeID: TypeID): RawType {
  if (typeID === context.voidTypeID) return RawType.Void;
  if (typeID === context.boolTypeID || typeID === context.intTypeID) return RawType.I32;
  return context.ptrType;
}

function defineLocal(context: Context, func: Func, scope: Scope, name: string, range: Range, typeID: TypeID): number {
  const local = findLocal(scope, name);
  if (local !== null) {
    appendToLog(context.log, range, `The name "${name}" is already used`);
    return local.index;
  }

  const index = createLocal(func, rawTypeForTypeID(context, typeID));
  scope.locals.set(name, {typeID, index});
  return index;
}

function compileVars(context: Context, parsed: Parsed): void {
  for (const decl of parsed.vars) {
    if (defineGlobal(context, decl.nameRange, decl.name, {kind: 'Var', varID: context.vars.length})) {
      let typeID = context.errorTypeID;
      let bytes = new Uint8Array(4);

      if (decl.type.kind.kind === 'Inferred') {
        typeID = context.intTypeID;
      } else {
        typeID = resolveTypeExpr(context, decl.type);

        // Only allow "int" for now
        if (typeID !== context.errorTypeID && typeID !== context.intTypeID) {
          const expected = context.types[context.intTypeID.index].name;
          const found = context.types[typeID.index].name;
          appendToLog(context.log, decl.type.range, `Expected type "${expected}" but found type "${found}"`);
          typeID = context.errorTypeID;
        }
      }

      // Restrict global variable initializers for now
      if (decl.value.kind.kind === 'Int') {
        new Int32Array(bytes.buffer)[0] = decl.value.kind.value;;
      } else {
        appendToLog(context.log, decl.value.range, `Invalid global variable initializer`);
        typeID = context.errorTypeID;
      }

      const index = context.code.globals.length;
      context.code.globals.push({name: decl.name, bytes});
      context.vars.push({name: decl.name, typeID, index});
    }
  }
}

export function compile(log: Log, parsed: Parsed, ptrType: RawType): Code {
  const context: Context = {
    librarySource: parsed.librarySource,
    code: createCode(),
    log,
    ptrType,
    types: [],
    defs: [],
    vars: [],
    globalScope: {
      globals: new Map,
      modules: new Map,
    },

    // Function-specific temporaries
    currentBlock: -1,
    loops: [],
    temporaries: [],
    controlFlowExprDepth: 0,
    stmtStartBlockIndex: 0,

    // Built-in types
    errorTypeID: {index: 0},
    voidTypeID: {index: 0},
    boolTypeID: {index: 0},
    intTypeID: {index: 0},
  };

  compileTypes(context, parsed);
  compileVars(context, parsed);
  compileDefs(context, parsed);
  return context.code;
}
