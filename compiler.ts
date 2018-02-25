import { Log, Range, appendToLog } from './log';
import { Parsed, TypeExpr, TypeDecl, CtorDecl } from './parser';
import { Code, Graph, RawType } from './ssa';

interface Field {
  name: string;
  typeID: number;
}

interface CtorData {
  name: string;
  fields: Field[];
}

interface TypeData {
  name: string;
  ctors: CtorData[];
  defaultCtor: number | null;
}

interface DefData {
  name: string;
  argTypeIDs: number[];
  retTypeID: number;
  graph: Graph | null;
}

type GlobalRef =
  {kind: 'Type', typeID: number} |
  {kind: 'Ctor', typeID: number, index: number} |
  {kind: 'Def', defID: number} |
  {kind: 'Var', varID: number};

interface Context {
  ptrType: RawType;
  types: TypeData[];
  defs: DefData[];
  globalScope: {[name: string]: GlobalRef};
  errorTypeID: number;
}

function defineGlobal(log: Log, context: Context, range: Range, name: string, ref: GlobalRef): boolean {
  if (name in context.globalScope) {
    appendToLog(log, range, `The name "${name}" is already used`);
    return false;
  }

  context.globalScope[name] = ref;
  return true;
}

function addTypeID(log: Log, context: Context, range: Range, name: string): number {
  const typeID = context.types.length;
  if (!defineGlobal(log, context, range, name, {kind: 'Type', typeID})) {
    return context.errorTypeID;
  }

  context.types.push({name, ctors: [], defaultCtor: null});
  return typeID;
}

function resolveToTypeID(log: Log, context: Context, type: TypeExpr): number {
  // Only support simple type names for now
  if (type.kind.kind !== 'Name') {
    appendToLog(log, type.range, `Unsupported type of kind ${type.kind.kind}`);
    return context.errorTypeID;
  }

  const name = type.kind.name;
  const ref = context.globalScope[name];

  // Check that the name exists
  if (!ref) {
    appendToLog(log, type.range, `There is no type named "${name}"`);
    return context.errorTypeID;
  }

  // People might try to use a constructor as a type
  if (ref.kind === 'Ctor') {
    const typeName = context.types[ref.typeID].name;
    appendToLog(log, type.range, `Use the type "${typeName}" instead of the constructor "${name}"`);
    return context.errorTypeID;
  }

  // The name must refer to a type
  if (ref.kind !== 'Type') {
    appendToLog(log, type.range, `The name "${name}" is not a type`);
    return context.errorTypeID;
  }

  return ref.typeID;
}

function compileTypes(log: Log, parsed: Parsed, context: Context): void {
  const list: [number, TypeDecl, CtorDecl[]][] = [];

  // Add type names first
  for (const decl of parsed.types) {
    if (decl.params.length === 0) {
      const typeID = addTypeID(log, context, decl.nameRange, decl.name);
      const data = context.types[typeID];
      const ctors: CtorDecl[] = [];

      // Also add constructors
      for (const ctor of decl.ctors) {
        const index = data.ctors.length;

        // Allow one constructor to share the name of the type
        if (ctor.name === decl.name && data.defaultCtor === null) {
          data.defaultCtor = data.ctors.length;
          data.ctors.push({name: ctor.name, fields: []});
          ctors.push(ctor);
        }

        // Otherwise the constructor must have a unique global name
        else if (defineGlobal(log, context, ctor.nameRange, ctor.name, {kind: 'Ctor', typeID, index})) {
          data.ctors.push({name: ctor.name, fields: []});
          ctors.push(ctor);
        }
      }

      list.push([typeID, decl, ctors]);
    }
  }

  // Add fields next
  for (const [typeID, type, ctors] of list) {
    const data = context.types[typeID];

    // Resolve field types now that all types have been defined
    for (let j = 0; j < ctors.length; j++) {
      const ctor = ctors[j];
      const fields = data.ctors[j].fields;

      // Turn constructor arguments into fields
      for (const arg of ctor.args) {
        const typeID = resolveToTypeID(log, context, arg.type);
        fields.push({name: arg.name, typeID});
      }
    }
  }
}

export function compile(log: Log, parsed: Parsed, ptrType: RawType): Context {
  const context: Context = {
    ptrType,
    types: [],
    defs: [],
    globalScope: {},
    errorTypeID: 0,
  };

  context.errorTypeID = addTypeID(log, context, {start: 0, end: 0}, '(error)');
  compileTypes(log, parsed, context);
  return context;
}
