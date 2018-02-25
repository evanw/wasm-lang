import { Log, Range, appendToLog } from './log';
import { Parsed, TypeExpr, CtorDecl, DefDecl, Stmt, Expr } from './parser';
import { Graph, RawType, createGraph, createLocal, ValueRef, createConstant, addLocalGet, Code, createBlock, addLocalSet, setJump } from './ssa';

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
}

interface TypeData {
  name: string;
  ctors: CtorData[];
  defaultCtor: number | null;
}

interface DefData {
  name: string;
  args: ArgData[];
  retTypeID: TypeID;
  graph: Graph | null;
}

type GlobalRef =
  {kind: 'Type', typeID: TypeID} |
  {kind: 'Ctor', typeID: TypeID, index: number} |
  {kind: 'Def', defID: number} |
  {kind: 'Var', varID: number};

interface Loop {
  continueTarget: number;
  breakTarget: number;
}

interface Context {
  code: Code;
  log: Log;
  ptrType: RawType;
  types: TypeData[];
  defs: DefData[];
  globalScope: {[name: string]: GlobalRef};

  // Function-specific temporaries
  currentBlock: number;
  loops: Loop[];

  // Built-in types
  errorTypeID: TypeID;
  voidTypeID: TypeID;
  boolTypeID: TypeID;
  intTypeID: TypeID;
  stringTypeID: TypeID;
}

function defineGlobal(context: Context, range: Range, name: string, ref: GlobalRef): boolean {
  if (name in context.globalScope) {
    appendToLog(context.log, range, `The name "${name}" is already used`);
    return false;
  }

  context.globalScope[name] = ref;
  return true;
}

function addTypeID(context: Context, range: Range, name: string): TypeID {
  const typeID: TypeID = {index: context.types.length};
  if (!defineGlobal(context, range, name, {kind: 'Type', typeID})) {
    return context.errorTypeID;
  }

  context.types.push({name, ctors: [], defaultCtor: null});
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
  const ref = context.globalScope[name];

  // Check that the name exists
  if (!ref) {
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
  const list: [TypeID, CtorDecl[]][] = [];

  // Add type names first
  for (const decl of parsed.types) {
    if (decl.params.length === 0) {
      const typeID = addTypeID(context, decl.nameRange, decl.name);
      const data = context.types[typeID.index];
      const ctors: CtorDecl[] = [];

      // Also add constructors
      for (const ctor of decl.ctors) {
        const index = data.ctors.length;

        // Allow one constructor to share the name of the type
        if (ctor.name === decl.name && data.defaultCtor === null) {
          data.defaultCtor = data.ctors.length;
          data.ctors.push({name: ctor.name, args: []});
          ctors.push(ctor);
        }

        // Otherwise the constructor must have a unique global name
        else if (defineGlobal(context, ctor.nameRange, ctor.name, {kind: 'Ctor', typeID, index})) {
          data.ctors.push({name: ctor.name, args: []});
          ctors.push(ctor);
        }
      }

      list.push([typeID, ctors]);
    }
  }

  // Bind built-in types
  context.boolTypeID = resolveTypeName(context, {start: 0, end: 0}, 'bool');
  context.intTypeID = resolveTypeName(context, {start: 0, end: 0}, 'int');
  context.stringTypeID = resolveTypeName(context, {start: 0, end: 0}, 'string');

  // Add fields next
  for (const [typeID, ctors] of list) {
    const data = context.types[typeID.index];

    // Resolve field types now that all types have been defined
    for (let j = 0; j < ctors.length; j++) {
      const ctor = ctors[j];
      const args = data.ctors[j].args;

      // Turn constructor arguments into fields
      for (const arg of ctor.args) {
        const typeID = resolveTypeExpr(context, arg.type);
        args.push({typeID, name: arg.name, isKey: arg.isKey, isRef: arg.isRef});
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
  locals: {[name: string]: Local};
}

function compileDefs(context: Context, parsed: Parsed): void {
  const list: [number, DefDecl][] = [];

  // Resolve all argument types and return types
  for (const def of parsed.defs) {
    if (defineGlobal(context, def.nameRange, def.name, {kind: 'Def', defID: context.defs.length})) {
      const args: ArgData[] = [];
      for (const arg of def.args) {
        const typeID = resolveTypeExpr(context, arg.type);
        args.push({typeID, name: arg.name, isKey: arg.isKey, isRef: arg.isRef});
      }
      const retTypeID = resolveTypeExpr(context, def.ret);
      list.push([context.defs.length, def]);
      context.defs.push({name: def.name, args, retTypeID, graph: null});
    }
  }

  // Compile each function
  for (const [defID, def] of list) {
    const data = context.defs[defID];
    const graph = createGraph(context.ptrType);
    const scope: Scope = {parent: null, locals: {}};

    // Add a local variable for each argument
    for (let i = 0; i < def.args.length; i++) {
      const arg = def.args[i];
      defineLocal(context, graph, scope, arg.name, arg.nameRange, data.args[i].typeID);
    }

    // Compile the body
    context.currentBlock = createBlock(graph);
    compileStmts(context, def.body, graph, scope, data.retTypeID);
    data.graph = graph;
  }
}

function compileStmts(context: Context, stmts: Stmt[], graph: Graph, parent: Scope, retTypeID: TypeID): void {
  const scope = {parent, locals: {}};

  for (const stmt of stmts) {
    switch (stmt.kind.kind) {
      case 'Var': {
        const type = stmt.kind.type;
        const value = stmt.kind.value;
        let typeID: TypeID;
        let initial: Result;

        if (type.kind.kind === 'Inferred') {
          initial = compileExpr(context, value, graph, scope, null);
          typeID = initial.typeID;

          // Forbid creating variables of certain types
          if (typeID === context.voidTypeID) {
            appendToLog(context.log, stmt.kind.nameRange, `Cannot create a variable of type "${context.types[typeID.index].name}"`);
            typeID = context.errorTypeID;
          }
        }

        else {
          typeID = resolveTypeExpr(context, type);
          initial = compileExpr(context, value, graph, scope, typeID);
        }

        const local = defineLocal(context, graph, scope, stmt.kind.name, stmt.kind.nameRange, typeID);
        addLocalSet(graph, context.currentBlock, local, initial.value);
        break;
      }

      case 'Return': {
        if (stmt.kind.value !== null) {
          const value = compileExpr(context, stmt.kind.value, graph, scope, retTypeID);
        }
        break;
      }

      case 'Break': {
        const count = stmt.kind.count;
        if (count >= 1 && count <= context.loops.length) {
          const after = createBlock(graph);
          setJump(graph, context.currentBlock, {kind: 'Goto', target: context.loops[count - 1].breakTarget});
          context.currentBlock = after;
        }
        break;
      }

      case 'Continue': {
        const count = stmt.kind.count;
        if (count >= 1 && count <= context.loops.length) {
          const after = createBlock(graph);
          setJump(graph, context.currentBlock, {kind: 'Goto', target: context.loops[count - 1].continueTarget});
          context.currentBlock = after;
        }
        break;
      }

      case 'If': {
        const test = compileExpr(context, stmt.kind.test, graph, scope, context.boolTypeID);
        compileStmts(context, stmt.kind.yes, graph, scope, retTypeID);
        compileStmts(context, stmt.kind.no, graph, scope, retTypeID);
        break;
      }

      case 'While': {
        const header = createBlock(graph);
        const body = createBlock(graph);
        const after = createBlock(graph);

        // Compile the test
        setJump(graph, context.currentBlock, {kind: 'Goto', target: header});
        context.currentBlock = header;
        const test = compileExpr(context, stmt.kind.test, graph, scope, context.boolTypeID);
        setJump(graph, context.currentBlock, {kind: 'Branch', value: test.value.ref, yes: body, no: after});

        // Compile the body
        context.currentBlock = body;
        context.loops.push({continueTarget: header, breakTarget: after});
        compileStmts(context, stmt.kind.body, graph, scope, retTypeID);
        context.loops.pop();
        setJump(graph, context.currentBlock, {kind: 'Goto', target: header});
        context.currentBlock = after;
        break;
      }

      case 'Assign': {
        const value = compileExpr(context, stmt.kind.value, graph, scope, null);
        break;
      }

      case 'Expr':
        compileExpr(context, stmt.kind.value, graph, scope, null);
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

function errorResult(context: Context, graph: Graph): Result {
  return {typeID: context.errorTypeID, value: createConstant(graph, 0)};
}

function compileArgs(context: Context, range: Range, exprs: Expr[], graph: Graph, scope: Scope, args: ArgData[]): Result[] {
  const results: Result[] = [];

  if (exprs.length !== args.length) {
    const expected = args.length === 1 ? '' : 's';
    const found = exprs.length === 1 ? '' : 's';
    appendToLog(context.log, range, `Expected ${args.length} argument${expected} but found ${exprs.length} argument${found}`);

    for (let i = 0; i < args.length; i++) {
      results.push(errorResult(context, graph));
    }
  }

  else {
    for (let i = 0; i < args.length; i++) {
      let expr = exprs[i];
      const arg = args[i];

      // Check for an expected key
      if (arg.isKey) {
        if (expr.kind.kind !== 'Key') {
          appendToLog(context.log, expr.range, `Expected the keyword "${arg.name}:" before this argument`);
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

      results.push(compileExpr(context, expr, graph, scope, arg.typeID));
    }
  }

  return results;
}

function compileExpr(context: Context, expr: Expr, graph: Graph, scope: Scope, castTo: TypeID | null): Result {
  let result = errorResult(context, graph);

  switch (expr.kind.kind) {
    case 'Bool':
      result = {
        typeID: context.boolTypeID,
        value: createConstant(graph, expr.kind.value ? 1 : 0),
      };
      break;

    case 'Int':
      result = {
        typeID: context.intTypeID,
        value: createConstant(graph, expr.kind.value),
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
          value: addLocalGet(graph, context.currentBlock, local.index),
        };
        break;
      }

      // Check for a global next
      let global = context.globalScope[name];
      if (global) {
        global = forwardToDefaultCtor(context, global);
        if (global.kind === 'Ctor') {
          const ctor = context.types[global.typeID.index].ctors[global.index];
          const args = compileArgs(context, expr.range, [], graph, scope, ctor.args);
          result = {
            typeID: global.typeID,
            value: createConstant(graph, 0),
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
      appendToLog(context.log, expr.range, `Unary operators are not currently implemented`);
      break;

    case 'Binary':
      appendToLog(context.log, expr.range, `Binary operators are not currently implemented`);
      break;

    case 'Call': {
      const name = expr.kind.name;

      // Check for a local first
      if (findLocal(scope, name) !== null) {
        appendToLog(context.log, expr.kind.nameRange, `Cannot call local variable "${name}"`);
        break;
      }

      // Check for a global next
      let global = context.globalScope[name];
      if (!global) {
        appendToLog(context.log, expr.kind.nameRange, `There is no symbol named "${name}" here`);
        break;
      }

      // Check for a constructor
      global = forwardToDefaultCtor(context, global);
      if (global.kind === 'Ctor') {
        const ctor = context.types[global.typeID.index].ctors[global.index];
        const args = compileArgs(context, expr.range, expr.kind.args, graph, scope, ctor.args);
        result = {
          typeID: global.typeID,
          value: createConstant(graph, 0),
        };
      }

      // Check for a function
      else if (global.kind === 'Def') {
        const def = context.defs[global.defID];
        const args = compileArgs(context, expr.range, expr.kind.args, graph, scope, def.args);
        result = {
          typeID: def.retTypeID,
          value: createConstant(graph, 0),
        };
      }

      else {
        appendToLog(context.log, expr.range, `Cannot use "${name}" as a value here`);
      }
      break;
    }

    case 'Dot': {
      const name = expr.kind.name;
      const target = compileExpr(context, expr.kind.target, graph, scope, null);
      if (target.typeID !== context.errorTypeID) {
        const type = context.types[target.typeID.index];
        if (type.ctors.length !== 1) {
          appendToLog(context.log, expr.kind.nameRange, `Cannot access the field "${name}" because the type "${type.name}" has multiple constructors`);
        } else {
          let found = false;
          for (const arg of type.ctors[0].args) {
            if (arg.name === name) {
              found = true;
              result = {
                typeID: arg.typeID,
                value: createConstant(graph, 0),
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
      throw new Error('Internal error');

    default: {
      const checkCovered: void = expr.kind;
      throw new Error('Internal error');
    }
  }

  if (castTo === null) {
    return result;
  }

  return cast(context, expr.range, result, castTo, graph);
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

function cast(context: Context, range: Range, result: Result, to: TypeID, graph: Graph): Result {
  if (result.typeID === context.errorTypeID || to === context.errorTypeID) {
    return errorResult(context, graph);
  }

  if (result.typeID !== to) {
    const expected = context.types[to.index].name;
    const found = context.types[result.typeID.index].name;
    appendToLog(context.log, range, `Expected type "${expected}" but found type "${found}"`);
    return errorResult(context, graph);
  }

  return result;
}

function findLocal(scope: Scope, name: string): Local | null {
  if (name in scope.locals) {
    return scope.locals[name];
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

function defineLocal(context: Context, graph: Graph, scope: Scope, name: string, range: Range, typeID: TypeID): number {
  const local = findLocal(scope, name);
  if (local !== null) {
    appendToLog(context.log, range, `The name "${name}" is already used`);
    return local.index;
  }

  const index = createLocal(graph, rawTypeForTypeID(context, typeID));
  scope.locals[name] = {typeID, index};
  return index;
}

export function compile(log: Log, parsed: Parsed, ptrType: RawType): Code {
  const context: Context = {
    code: {funcs: [], globals: []},
    log,
    ptrType,
    types: [],
    defs: [],
    globalScope: {},

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

  context.errorTypeID = addTypeID(context, {start: 0, end: 0}, '(error)');
  context.voidTypeID = addTypeID(context, {start: 0, end: 0}, '(void)');
  compileTypes(context, parsed);
  compileDefs(context, parsed);
  return context.code;
}
