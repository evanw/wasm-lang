import { Log, Range, appendToLog } from './log';
import { Parsed, TypeExpr, CtorDecl, DefDecl, Stmt, Expr, BinOp, UnOp, Tag, Pattern } from './parser';
import {
  addIns,
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
  setJump,
  setNext,
  unwrapRef,
  ValueRef,
  addMemGet,
  addMemSet,
  createCode,
} from './ssa';
import { assert, align } from './util';

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
  loops: number[];

  // Built-in types
  errorTypeID: TypeID;
  voidTypeID: TypeID;
  boolTypeID: TypeID;
  intTypeID: TypeID;
  stringTypeID: TypeID;
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
    defaultCtor: null,
    hasRefCount: true,
    tagOffset: null,
    fieldSize: fieldBytes,
    fieldAlign: fieldBytes,
    allocSize: 1,
  });
  return typeID;
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

  // Bind built-in types
  context.boolTypeID = resolveTypeName(context, {source: -1, start: 0, end: 0}, 'bool');
  context.intTypeID = resolveTypeName(context, {source: -1, start: 0, end: 0}, 'int');
  context.stringTypeID = resolveTypeName(context, {source: -1, start: 0, end: 0}, 'string');

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
    appendToLog(context.log, tag.range, `The "@intrinsic" tag takes one argument`);
    return null;
  }

  const arg = tag.args[0];
  if (arg.kind.kind !== 'String') {
    appendToLog(context.log, arg.range, `The "@intrinsic" argument must be a string`);
    return null;
  }

  return arg.kind.value;
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

    // Add a local variable for each argument
    for (let i = 0; i < def.args.length; i++) {
      const arg = def.args[i];
      const typeID = data.args[i].typeID;
      defineLocal(context, func, scope, arg.name, arg.nameRange, typeID);
      func.argTypes.push(rawTypeForTypeID(context, typeID));
    }

    // Compile the body
    context.currentBlock = createBlock(func);
    func.retType = rawTypeForTypeID(context, data.retTypeID);
    if (def.body !== null) {
      compileStmts(context, def.body, func, scope, data.retTypeID);
    }

    // Check for a missing return statement
    if (data.retTypeID !== context.voidTypeID) {
      const metas = buildBlockMetas(func);
      if (hasMissingReturn(func, metas)) {
        appendToLog(context.log, def.nameRange, `Not all code paths return a value`);
      }
    }
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
  const equals = addIns(func, context.currentBlock, {kind: 'Eq32', left: tag.ref, right: createConstant(func, global.index).ref});

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
    const value = addMemGet(func, trueBlock, result.value,
      ctor.fieldOffsets[i], context.types[arg.typeID.index].fieldSize);
    addLocalSet(func, trueBlock, local, value);
  }

  return {
    typeID: context.boolTypeID,
    value: equals,
  };
}

function compileStmts(context: Context, stmts: Stmt[], func: Func, parent: Scope, retTypeID: TypeID): void {
  const scope = createScope(parent);

  for (const stmt of stmts) {
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
        addLocalSet(func, context.currentBlock, local, initial.value);
        break;
      }

      case 'Return': {
        // Return a value
        if (stmt.kind.value !== null) {
          if (retTypeID === context.voidTypeID) {
            appendToLog(context.log, stmt.kind.value.range, `Unexpected return value in a function without a return type`);
          } else {
            const result = compileExpr(context, stmt.kind.value, func, scope, retTypeID);
            setJump(func, context.currentBlock, {kind: 'Return', value: result.value.ref});
            context.currentBlock = createBlock(func);
          }
        }

        // Return nothing
        else {
          if (retTypeID !== context.voidTypeID) {
            appendToLog(context.log, stmt.range, `Must return a value of type "${context.types[retTypeID.index].name}"`);
          } else {
            setJump(func, context.currentBlock, {kind: 'ReturnVoid'});
            context.currentBlock = createBlock(func);
          }
        }
        break;
      }

      case 'Break': {
        const count = stmt.kind.count;

        // The parser handles out-of-bounds error reporting
        if (count >= 1 && count <= context.loops.length) {
          const target: JumpTarget = {kind: 'Next', parent: context.loops[context.loops.length - count]};
          setJump(func, context.currentBlock, {kind: 'Goto', target});
          context.currentBlock = createBlock(func);
        }
        break;
      }

      case 'Continue': {
        const count = stmt.kind.count;

        // The parser handles out-of-bounds error reporting
        if (count >= 1 && count <= context.loops.length) {
          const target: JumpTarget = {kind: 'Loop', parent: context.loops[context.loops.length - count]};
          setJump(func, context.currentBlock, {kind: 'Goto', target});
          context.currentBlock = createBlock(func);
        }
        break;
      }

      case 'If': {
        const thenBlock = createBlock(func);
        const thenScope = createScope(scope);
        const test = compileMatchOrExpr(context, func, thenScope, stmt.kind.test, thenBlock);
        const parent = context.currentBlock;

        // Without an else
        if (stmt.kind.no.length === 0) {
          setJump(func, parent, {
            kind: 'Branch',
            value: test.value.ref,
            yes: {kind: 'Child', index: thenBlock},
            no: {kind: 'Next', parent},
          });

          // Then branch
          context.currentBlock = thenBlock;
          compileStmts(context, stmt.kind.yes, func, thenScope, retTypeID);
          setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Next', parent}});
        }

        // With an else
        else {
          const noBlock = createBlock(func);
          setJump(func, parent, {
            kind: 'Branch',
            value: test.value.ref,
            yes: {kind: 'Child', index: thenBlock},
            no: {kind: 'Child', index: noBlock},
          });

          // Then branch
          context.currentBlock = thenBlock;
          compileStmts(context, stmt.kind.yes, func, thenScope, retTypeID);
          setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Next', parent}});

          // Else branch
          context.currentBlock = noBlock;
          compileStmts(context, stmt.kind.no, func, scope, retTypeID);
          setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Next', parent}});
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
        setJump(func, previous, {kind: 'Goto', target: {kind: 'Next', parent: previous}});
        setNext(func, previous, loop);
        setJump(func, loop, {kind: 'Goto', target: {kind: 'Child', index: header}});
        context.currentBlock = header;

        // Compile the test (special-case "while true" for "return" statement checking)
        const testExpr = stmt.kind.test;
        const body = createBlock(func);
        const bodyScope = createScope(scope);
        if (testExpr.kind.kind === 'Bool' && testExpr.kind.value === true) {
          setJump(func, header, {kind: 'Goto', target: {kind: 'Next', parent: header}});
        } else {
          const test = compileMatchOrExpr(context, func, bodyScope, stmt.kind.test, body);
          setJump(func, context.currentBlock, {
            kind: 'Branch',
            value: test.value.ref,
            yes: {kind: 'Next', parent: context.currentBlock},
            no: {kind: 'Next', parent: loop},
          });
        }

        // Compile the body
        setNext(func, context.currentBlock, body);
        context.currentBlock = body;
        context.loops.push(loop);
        compileStmts(context, stmt.kind.body, func, bodyScope, retTypeID);
        context.loops.pop();
        setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Loop', parent: loop}});

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
          addLocalSet(func, context.currentBlock, local.index, result.value);
          break;
        }

        // Check for a global next
        const global = findGlobal(context.globalScope, stmt.range.source, name);
        if (global !== null) {
          if (global.kind === 'Var') {
            const data = context.vars[global.varID];
            const ptr = addIns(func, context.currentBlock, {kind: 'PtrGlobal', index: data.index});
            const size = context.types[data.typeID.index].fieldSize;
            const result = compileExpr(context, stmt.kind.value, func, scope, data.typeID);
            addMemSet(func, context.currentBlock, ptr, 0, size, result.value);
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
        break;

      default: {
        const checkCovered: void = stmt.kind;
        throw new Error('Internal error');
      }
    }
  }
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

function allocType(context: Context, func: Func, type: TypeData, ctorIndex: number, args: Result[]): ValueRef {
  const size = createConstant(func, type.allocSize).ref;
  const ptr = addIns(func, context.currentBlock, {kind: 'MemAlloc', size});

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
    const size = context.types[ctor.args[i].typeID.index].fieldSize;
    addMemSet(func, context.currentBlock, ptr, ctor.fieldOffsets[i], size, args[i].value);
  }

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
            value: allocType(context, func, type, global.index, args),
          };
        } else if (global.kind === 'Var') {
          const data = context.vars[global.varID];
          const ptr = addIns(func, context.currentBlock, {kind: 'PtrGlobal', index: data.index});
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
          const value = compileExpr(context, expr.kind.value, func, scope, context.intTypeID);
          result = {
            typeID: context.intTypeID,
            value: addIns(func, context.currentBlock, {
              kind: 'Xor32',
              left: unwrapRef(func, context.currentBlock, value.value),
              right: createConstant(func, -1).ref,
            }),
          };
          break;
        }

        case UnOp.Neg: {
          const value = compileExpr(context, expr.kind.value, func, scope, context.intTypeID);
          result = {
            typeID: context.intTypeID,
            value: addIns(func, context.currentBlock, {
              kind: 'Sub32',
              left: createConstant(func, 0).ref,
              right: unwrapRef(func, context.currentBlock, value.value),
            }),
          };
          break;
        }

        case UnOp.Not: {
          const value = compileExpr(context, expr.kind.value, func, scope, context.boolTypeID);
          result = {
            typeID: context.boolTypeID,
            value: addIns(func, context.currentBlock, {
              kind: 'Eq32',
              left: unwrapRef(func, context.currentBlock, value.value),
              right: createConstant(func, 0).ref,
            }),
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
          value: allocType(context, func, type, global.index, args),
        };
      }

      // Check for a function
      else if (global.kind === 'Def') {
        const def = context.defs[global.defID];
        const retType = rawTypeForTypeID(context, def.retTypeID);
        const results = compileArgs(context, expr.range, expr.kind.args, func, scope, def.args);
        const args: InsRef[] = [];
        for (const result of results) {
          args.push(unwrapRef(func, context.currentBlock, result.value));
        }
        switch (def.kind.kind) {
          case 'Func':
            result = {
              typeID: def.retTypeID,
              value: addIns(func, context.currentBlock, {kind: 'Call', index: def.kind.index, args, retType}),
            };
            break;

          case 'Import':
            result = {
              typeID: def.retTypeID,
              value: addIns(func, context.currentBlock, {kind: 'CallImport', index: def.kind.index, args, retType}),
            };
            break;

          case 'Intrinsic':
            result = {
              typeID: def.retTypeID,
              value: addIns(func, context.currentBlock, {kind: 'CallIntrinsic', name: def.kind.name, args, retType}),
            };
            break;

          default: {
            const checkCovered: void = def.kind;
            throw new Error('Internal error');
          }
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

  const left = unwrapRef(func, context.currentBlock, l.value);
  const right = unwrapRef(func, context.currentBlock, r.value);
  let ins: Ins;

  switch (op) {
    case BinOp.Eq: ins = {kind: 'Eq32', left, right}; break;
    case BinOp.NotEq: ins = {kind: 'NotEq32', left, right}; break;
    default: throw new Error('Internal error');
  }

  return {
    typeID: context.boolTypeID,
    value: addIns(func, context.currentBlock, ins),
  };
}

function compileCompare32(context: Context, func: Func, scope: Scope, op: BinOp, leftExpr: Expr, rightExpr: Expr): Result {
  const l = compileExpr(context, leftExpr, func, scope, context.intTypeID);
  const r = compileExpr(context, rightExpr, func, scope, context.intTypeID);
  const left = unwrapRef(func, context.currentBlock, l.value);
  const right = unwrapRef(func, context.currentBlock, r.value);
  let ins: Ins;

  switch (op) {
    case BinOp.Lt: ins = {kind: 'Lt32S', left, right}; break;
    case BinOp.LtEq: ins = {kind: 'LtEq32S', left, right}; break;
    case BinOp.Gt: ins = {kind: 'Lt32S', left: right, right: left}; break;
    case BinOp.GtEq: ins = {kind: 'LtEq32S', left: right, right: left}; break;
    default: throw new Error('Internal error');
  }

  return {
    typeID: context.boolTypeID,
    value: addIns(func, context.currentBlock, ins),
  };
}

function compileMath32(context: Context, func: Func, scope: Scope, op: BinOp, leftExpr: Expr, rightExpr: Expr): Result {
  const l = compileExpr(context, leftExpr, func, scope, context.intTypeID);
  const r = compileExpr(context, rightExpr, func, scope, context.intTypeID);
  const left = unwrapRef(func, context.currentBlock, l.value);
  const right = unwrapRef(func, context.currentBlock, r.value);
  let ins: Ins;

  switch (op) {
    case BinOp.BitOr: ins = {kind: 'Or32', left, right}; break;
    case BinOp.BitXor: ins = {kind: 'Xor32', left, right}; break;
    case BinOp.BitAnd: ins = {kind: 'And32', left, right}; break;
    case BinOp.Add: ins = {kind: 'Add32', left, right}; break;
    case BinOp.Sub: ins = {kind: 'Sub32', left, right}; break;
    case BinOp.Mul: ins = {kind: 'Mul32', left, right}; break;
    case BinOp.Div: ins = {kind: 'Div32S', left, right}; break;
    case BinOp.Shl: ins = {kind: 'Shl32', left, right}; break;
    case BinOp.Shr: ins = {kind: 'Shr32S', left, right}; break;
    default: throw new Error('Internal error');
  }

  return {
    typeID: context.intTypeID,
    value: addIns(func, context.currentBlock, ins),
  };
}

function compileShortCircuit(context: Context, func: Func, scope: Scope, op: BinOp, leftExpr: Expr, rightExpr: Expr): Result {
  // Compile the left
  const local = createLocal(func, RawType.I32);
  const left = compileExpr(context, leftExpr, func, scope, context.boolTypeID);
  const leftEnd = context.currentBlock;
  addLocalSet(func, leftEnd, local, left.value);

  // Skip the right if the condition was met
  const rightStart = createBlock(func);
  const next = createBlock(func);
  setJump(func, leftEnd, {
    kind: 'Branch',
    value: left.value.ref,
    yes: op === BinOp.And ? {kind: 'Child', index: rightStart} : {kind: 'Next', parent: leftEnd},
    no: op === BinOp.And ? {kind: 'Next', parent: leftEnd} : {kind: 'Child', index: rightStart},
  });
  setNext(func, leftEnd, next);

  // Otherwise evaluate the right
  context.currentBlock = rightStart;
  const right = compileExpr(context, rightExpr, func, scope, context.boolTypeID);
  const rightEnd = context.currentBlock;
  addLocalSet(func, rightEnd, local, right.value);
  setJump(func, rightEnd, {
    kind: 'Goto',
    target: {kind: 'Next', parent: leftEnd},
  });

  // Merge the control flow for following expressions
  context.currentBlock = next;
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

    // Built-in types
    errorTypeID: {index: 0},
    voidTypeID: {index: 0},
    boolTypeID: {index: 0},
    intTypeID: {index: 0},
    stringTypeID: {index: 0},
  };

  context.errorTypeID = addTypeID(context, {source: -1, start: 0, end: 0}, '(error)', 0);
  context.voidTypeID = addTypeID(context, {source: -1, start: 0, end: 0}, '(void)', 0);
  compileTypes(context, parsed);
  compileVars(context, parsed);
  compileDefs(context, parsed);
  return context.code;
}
