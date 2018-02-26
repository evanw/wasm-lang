import { Log, Range, appendToLog } from './log';
import { Parsed, TypeExpr, CtorDecl, DefDecl, Stmt, Expr, BinOp, UnOp } from './parser';
import { Func, RawType, createFunc, createLocal, ValueRef, createConstant, addLocalGet,
  Code, createBlock, addLocalSet, setJump, addIns, InsRef, unwrapRef, setNext, Ins } from './ssa';

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
}

type GlobalRef =
  {kind: 'Type', typeID: TypeID} |
  {kind: 'Ctor', typeID: TypeID, index: number} |
  {kind: 'Def', defID: number} |
  {kind: 'Var', varID: number};

interface Context {
  code: Code;
  log: Log;
  ptrType: RawType;
  types: TypeData[];
  defs: DefData[];
  globalScope: Map<string, GlobalRef>;

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

function defineGlobal(context: Context, range: Range, name: string, ref: GlobalRef): boolean {
  if (context.globalScope.get(name) !== undefined) {
    appendToLog(context.log, range, `The name "${name}" is already used`);
    return false;
  }

  context.globalScope.set(name, ref);
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
  const ref = context.globalScope.get(name);

  // Check that the name exists
  if (ref === undefined) {
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
  locals: Map<string, Local>;
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
      context.defs.push({name: def.name, args, retTypeID});
    }
  }

  // Compile each function
  for (const [defID, def] of list) {
    const data = context.defs[defID];
    const func = createFunc(def.name, context.ptrType);
    const scope: Scope = {parent: null, locals: new Map()};

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
    compileStmts(context, def.body, func, scope, data.retTypeID);
    context.code.funcs.push(func);
  }
}

function compileStmts(context: Context, stmts: Stmt[], func: Func, parent: Scope, retTypeID: TypeID): void {
  const scope: Scope = {parent, locals: new Map()};

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
          const after = createBlock(func);
          setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Next', parent: context.loops[count - 1]}});
          context.currentBlock = after;
        }
        break;
      }

      case 'Continue': {
        const count = stmt.kind.count;

        // The parser handles out-of-bounds error reporting
        if (count >= 1 && count <= context.loops.length) {
          const after = createBlock(func);
          setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Loop', parent: context.loops[count - 1]}});
          context.currentBlock = after;
        }
        break;
      }

      case 'If': {
        // Compile the test
        const test = compileExpr(context, stmt.kind.test, func, scope, context.boolTypeID);
        const parent = context.currentBlock;
        const yes = createBlock(func);

        // Without an else
        if (stmt.kind.no.length === 0) {
          setJump(func, parent, {
            kind: 'Branch',
            value: test.value.ref,
            yes: {kind: 'Child', index: yes},
            no: {kind: 'Next', parent},
          });

          // Then branch
          context.currentBlock = yes;
          compileStmts(context, stmt.kind.yes, func, scope, retTypeID);
          setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Next', parent}});
        }

        // With an else
        else {
          const no = createBlock(func);
          setJump(func, parent, {
            kind: 'Branch',
            value: test.value.ref,
            yes: {kind: 'Child', index: yes},
            no: {kind: 'Child', index: no},
          });

          // Then branch
          context.currentBlock = yes;
          compileStmts(context, stmt.kind.yes, func, scope, retTypeID);
          setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Next', parent}});

          // Else branch
          context.currentBlock = no;
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

        // Compile the test
        const test = compileExpr(context, stmt.kind.test, func, scope, context.boolTypeID);
        setJump(func, context.currentBlock, {
          kind: 'Branch',
          value: test.value.ref,
          yes: {kind: 'Next', parent: context.currentBlock},
          no: {kind: 'Next', parent: loop},
        });

        // Compile the body
        const body = createBlock(func);
        setNext(func, context.currentBlock, body);
        context.currentBlock = body;
        context.loops.push(loop);
        compileStmts(context, stmt.kind.body, func, scope, retTypeID);
        context.loops.pop();
        setJump(func, context.currentBlock, {kind: 'Goto', target: {kind: 'Loop', parent: loop}});

        // Merge the control flow for following statements
        const next = createBlock(func);
        setNext(func, loop, next);
        context.currentBlock = next;
        break;
      }

      case 'Assign': {
        const value = compileExpr(context, stmt.kind.value, func, scope, null);
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

      results.push(compileExpr(context, expr, func, scope, arg.typeID));
    }
  }

  return results;
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
      let global = context.globalScope.get(name);
      if (global !== undefined) {
        global = forwardToDefaultCtor(context, global);
        if (global.kind === 'Ctor') {
          const ctor = context.types[global.typeID.index].ctors[global.index];
          const args = compileArgs(context, expr.range, [], func, scope, ctor.args);
          result = {
            typeID: global.typeID,
            value: createConstant(func, 0),
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
        case BinOp.Div: {
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
      let global = context.globalScope.get(name);
      if (global === undefined) {
        appendToLog(context.log, expr.kind.nameRange, `There is no symbol named "${name}" here`);
        break;
      }

      // Check for a constructor
      global = forwardToDefaultCtor(context, global);
      if (global.kind === 'Ctor') {
        const ctor = context.types[global.typeID.index].ctors[global.index];
        const args = compileArgs(context, expr.range, expr.kind.args, func, scope, ctor.args);
        result = {
          typeID: global.typeID,
          value: createConstant(func, 0),
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
        result = {
          typeID: def.retTypeID,
          value: addIns(func, context.currentBlock, {kind: 'Call', index: global.defID, args, retType}),
        };
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
          let found = false;
          for (const arg of type.ctors[0].args) {
            if (arg.name === name) {
              found = true;
              result = {
                typeID: arg.typeID,
                value: createConstant(func, 0),
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
    appendToLog(context.log, {start: leftExpr.range.start, end: rightExpr.range.end},
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

export function compile(log: Log, parsed: Parsed, ptrType: RawType): Code {
  const context: Context = {
    code: {funcs: [], globals: []},
    log,
    ptrType,
    types: [],
    defs: [],
    globalScope: new Map(),

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
