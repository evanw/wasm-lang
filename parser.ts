import { Log, Range, appendToLog } from './log';
import { Token, currentText, spanSince, advance, Lexer, currentRange, createLexer } from './lexer';

export interface Parsed {
  vars: VarDecl[];
  defs: DefDecl[];
  types: TypeDecl[];
}

export interface VarDecl {
  range: Range;
  name: string;
  nameRange: Range;
  type: TypeExpr;
  value: Expr;
}

export interface DefDecl {
  range: Range;
  name: string;
  nameRange: Range;
  params: ParamDecl[];
  args: ArgDecl[];
  ret: TypeExpr;
  body: Stmt[]
}

export interface TypeDecl {
  range: Range;
  name: string;
  nameRange: Range;
  params: ParamDecl[];
  ctors: CtorDecl[]
}

export interface ParamDecl {
  range: Range;
  name: string;
}

export interface CtorDecl {
  range: Range;
  name: string;
  nameRange: Range;
  args: ArgDecl[];
}

export interface ArgDecl {
  range: Range;
  name: string;
  nameRange: Range;
  isKey: boolean;
  isRef: boolean;
  type: TypeExpr;
}

export interface Stmt {
  range: Range;
  kind: StmtKind;
}

export type StmtKind =
  {kind: 'Var', name: string, type: TypeExpr, value: Expr} |
  {kind: 'Return', value: Expr | null} |
  {kind: 'If', test: Expr, yes: Stmt[], no: Stmt[]} |
  {kind: 'While', test: Expr, body: Stmt[]} |
  {kind: 'Assign', target: Expr, value: Expr} |
  {kind: 'Expr', value: Expr};

export interface Expr {
  range: Range;
  kind: ExprKind;
}

export enum UnOp {
  Not,
  Neg,
}

export enum BinOp {
  Add,
  Sub,
  Mul,
  Div,
  Eq,
  NotEq,
  Lt,
  Gt,
  LtEq,
  GtEq,
  And,
  Or,
}

export type ExprKind =
  {kind: 'Bool', value: boolean} |
  {kind: 'Int', value: number} |
  {kind: 'Name', value: string} |
  {kind: 'String', value: string} |
  {kind: 'Ref', value: Expr} |
  {kind: 'Key', name: string, nameRange: Range, value: Expr} |
  {kind: 'Array', values: Expr[]} |
  {kind: 'Unary', op: UnOp, value: Expr} |
  {kind: 'Binary', op: BinOp, left: Expr, right: Expr} |
  {kind: 'Call', name: string, nameRange: Range, args: Expr[]} |
  {kind: 'Dot', target: Expr, name: string} |
  {kind: 'Index', target: Expr, value: Expr};

export interface TypeExpr {
  range: Range;
  kind: TypeExprKind;
}

export type TypeExprKind =
  {kind: 'Void'} |
  {kind: 'Inferred'} |
  {kind: 'Array', type: TypeExpr} |
  {kind: 'Option', type: TypeExpr} |
  {kind: 'Name', name: string} |
  {kind: 'Generic', name: string, params: TypeExpr[]};

function eat(lexer: Lexer, token: Token): boolean {
  if (lexer.token === token) {
    advance(lexer);
    return true;
  }

  return false;
}

function expect(lexer: Lexer, token: Token): boolean {
  if (eat(lexer, token)) {
    return true;
  }

  appendToLog(lexer.log, currentRange(lexer), `Expected ${Token[token]} but found ${Token[lexer.token]}`);
  return false;
}

function unexpected(lexer: Lexer): void {
  appendToLog(lexer.log, currentRange(lexer), `Unexpected ${Token[lexer.token]}`);
}

function parseTypeSuffix(lexer: Lexer, type: TypeExpr): TypeExpr | null {
  while (true) {
    if (eat(lexer, Token.OpenBracket)) {
      if (!expect(lexer, Token.CloseBracket)) return null;
      type = {range: spanSince(lexer, type.range.start), kind: {kind: 'Array', type}};
    } else if (eat(lexer, Token.QuestionMark)) {
      type = {range: spanSince(lexer, type.range.start), kind: {kind: 'Option', type}};
    } else {
      return type;
    }
  }
}

function parseType(lexer: Lexer): TypeExpr | null {
  const start = lexer.start;
  const name = currentText(lexer);
  if (!expect(lexer, Token.Identifier)) return null;
  if (!eat(lexer, Token.LessThan)) {
    return parseTypeSuffix(lexer, {range: spanSince(lexer, start), kind: {kind: 'Name', name}});
  }

  const params: TypeExpr[] = [];
  eat(lexer, Token.Newline);

  while (lexer.token !== Token.GreaterThan) {
    const param = parseType(lexer);
    if (param === null) return null;
    params.push(param);
    if (!eat(lexer, Token.Comma)) break;
    eat(lexer, Token.Newline);
  }

  eat(lexer, Token.Newline);
  if (!expect(lexer, Token.GreaterThan)) return null;
  return parseTypeSuffix(lexer, {range: spanSince(lexer, start), kind: {kind: 'Generic', name, params}});
}

function parsePrefix(lexer: Lexer): Expr | null {
  const start = lexer.start;

  switch (lexer.token!) {
    case Token.False: {
      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'Bool', value: false}};
    }

    case Token.True: {
      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'Bool', value: true}};
    }

    case Token.Identifier: {
      const value = currentText(lexer);
      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'Name', value}};
    }

    case Token.Integer: {
      const value = +currentText(lexer);
      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'Int', value}};
    }

    case Token.Character: {
      const text = currentText(lexer);
      let value = 0;

      if (text.length === 3) {
        value = text.charCodeAt(1);
      } else if (text.length === 4 && text[1] === '\\') {
        switch (text[2]) {
          case '0': value = 0; break;
          case 'b': value = '\b'.charCodeAt(0); break;
          case 'n': value = '\n'.charCodeAt(0); break;
          case 'r': value = '\r'.charCodeAt(0); break;
          case 't': value = '\t'.charCodeAt(0); break;
          default: appendToLog(lexer.log, currentRange(lexer), `Invalid character escape sequence`); break;
        }
      } else {
        appendToLog(lexer.log, currentRange(lexer), `Invalid character literal`);
      }

      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'Int', value}};
    }

    case Token.String: {
      const value = currentText(lexer);
      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'String', value}};
    }

    case Token.ExclamationMark: {
      advance(lexer);
      const value = parseExpr(lexer, LEVEL_PREFIX);
      if (value === null) return null;
      return {range: spanSince(lexer, start), kind: {kind: 'Unary', op: UnOp.Not, value}};
    }

    case Token.Minus: {
      advance(lexer);
      const value = parseExpr(lexer, LEVEL_PREFIX);
      if (value === null) return null;
      return {range: spanSince(lexer, start), kind: {kind: 'Unary', op: UnOp.Neg, value}};
    }

    case Token.OpenParenthesis: {
      advance(lexer);
      const value = parseExpr(lexer, LEVEL_LOWEST);
      if (value === null || !expect(lexer, Token.CloseParenthesis)) return null;
      return {range: spanSince(lexer, start), kind: value.kind};
    }

    case Token.OpenBracket: {
      const values: Expr[] = [];
      advance(lexer);
      eat(lexer, Token.Newline);

      while (lexer.token !== Token.CloseBracket) {
        const value = parseExpr(lexer, LEVEL_LOWEST);
        if (value === null) return null;
        values.push(value);
        if (!eat(lexer, Token.Comma)) break;
        eat(lexer, Token.Newline);
      }

      eat(lexer, Token.Newline);
      if (!expect(lexer, Token.CloseBracket)) return null;
      return {range: spanSince(lexer, start), kind: {kind: 'Array', values}};
    }
  }

  unexpected(lexer);
  return null;
}

const LEVEL_LOWEST = 0;
const LEVEL_LOGICAL_OR = 1;
const LEVEL_LOGICAL_AND = 2;
const LEVEL_EQUALITY = 3;
const LEVEL_COMPARE = 4;
const LEVEL_ADD = 5;
const LEVEL_MULTIPLY = 6;
const LEVEL_PREFIX = 7;

function parseBinary(lexer: Lexer, left: Expr, op: BinOp, level: number): Expr | null {
  advance(lexer);
  const right = parseExpr(lexer, level);
  if (right === null) return null;
  return {range: spanSince(lexer, left.range.start), kind: {kind: 'Binary', op, left, right}};
}

function parseExpr(lexer: Lexer, level: number): Expr | null {
  const start = lexer.start;
  let left = parsePrefix(lexer);

  while (true) {
    if (left === null) return null;

    switch (lexer.token!) {
      case Token.BarBar: {
        if (level >= LEVEL_LOGICAL_OR) return left;
        left = parseBinary(lexer, left, BinOp.Or, LEVEL_LOGICAL_OR);
        break;
      }

      case Token.AmpersandAmpersand: {
        if (level >= LEVEL_LOGICAL_AND) return left;
        left = parseBinary(lexer, left, BinOp.And, LEVEL_LOGICAL_AND);
        break;
      }

      case Token.EqualsEquals: {
        if (level >= LEVEL_EQUALITY) return left;
        left = parseBinary(lexer, left, BinOp.Eq, LEVEL_EQUALITY);
        break;
      }

      case Token.ExclamationMarkEquals: {
        if (level >= LEVEL_EQUALITY) return left;
        left = parseBinary(lexer, left, BinOp.NotEq, LEVEL_EQUALITY);
        break;
      }

      case Token.LessThan: {
        if (level >= LEVEL_COMPARE) return left;
        left = parseBinary(lexer, left, BinOp.Lt, LEVEL_COMPARE);
        break;
      }

      case Token.GreaterThan: {
        if (level >= LEVEL_COMPARE) return left;
        left = parseBinary(lexer, left, BinOp.Gt, LEVEL_COMPARE);
        break;
      }

      case Token.LessThanEquals: {
        if (level >= LEVEL_COMPARE) return left;
        left = parseBinary(lexer, left, BinOp.LtEq, LEVEL_COMPARE);
        break;
      }

      case Token.GreaterThanEquals: {
        if (level >= LEVEL_COMPARE) return left;
        left = parseBinary(lexer, left, BinOp.GtEq, LEVEL_COMPARE);
        break;
      }

      case Token.Plus: {
        if (level >= LEVEL_ADD) return left;
        left = parseBinary(lexer, left, BinOp.Add, LEVEL_ADD);
        break;
      }

      case Token.Minus: {
        if (level >= LEVEL_ADD) return left;
        left = parseBinary(lexer, left, BinOp.Sub, LEVEL_ADD);
        break;
      }

      case Token.Asterisk: {
        if (level >= LEVEL_MULTIPLY) return left;
        left = parseBinary(lexer, left, BinOp.Mul, LEVEL_MULTIPLY);
        break;
      }

      case Token.Slash: {
        if (level >= LEVEL_MULTIPLY) return left;
        left = parseBinary(lexer, left, BinOp.Div, LEVEL_MULTIPLY);
        break;
      }

      case Token.Dot: {
        advance(lexer);
        const name = currentText(lexer);
        if (!expect(lexer, Token.Identifier)) return null;
        left = {range: spanSince(lexer, start), kind: {kind: 'Dot', target: left, name}};
        break;
      }

      case Token.OpenBracket: {
        advance(lexer);
        const value = parseExpr(lexer, LEVEL_LOWEST);
        if (value === null || !expect(lexer, Token.CloseBracket)) return null;
        left = {range: spanSince(lexer, start), kind: {kind: 'Index', target: left, value}};
        break;
      }

      case Token.OpenParenthesis: {
        const args: Expr[] = [];
        advance(lexer);
        eat(lexer, Token.Newline);

        while (lexer.token !== Token.CloseParenthesis) {
          const start = lexer.start;
          const isRef = eat(lexer, Token.Ampersand);
          const isMaybeKey = lexer.token === Token.Identifier;
          let value = parseExpr(lexer, LEVEL_LOWEST);
          if (value === null) return null;
          if (isRef) value = {range: spanSince(lexer, start), kind: {kind: 'Ref', value}};
          else if (isMaybeKey && value.kind.kind === 'Name' && eat(lexer, Token.Colon)) {
            const name = value.kind.value;
            const nameRange = spanSince(lexer, value.range.start);
            const start2 = lexer.start;
            const isRef2 = eat(lexer, Token.Ampersand);
            value = parseExpr(lexer, LEVEL_LOWEST);
            if (value === null) return null;
            if (isRef2) value = {range: spanSince(lexer, start2), kind: {kind: 'Ref', value}};
            value = {range: spanSince(lexer, start), kind: {kind: 'Key', name, nameRange, value}};
          }
          args.push(value);
          if (!eat(lexer, Token.Comma)) break;
          eat(lexer, Token.Newline);
        }

        eat(lexer, Token.Newline);
        if (!expect(lexer, Token.CloseParenthesis)) return null;
        if (left.kind.kind === 'Name') {
          left = {range: spanSince(lexer, start), kind: {kind: 'Call', name: left.kind.value, nameRange: left.range, args}};
        } else {
          appendToLog(lexer.log, left.range, 'Expected a function name');
        }
        break;
      }

      default:
        return left;
    }
  }
}

function parseArgs(lexer: Lexer): ArgDecl[] | null {
  const args: ArgDecl[] = [];
  if (!expect(lexer, Token.OpenParenthesis)) return null;
  eat(lexer, Token.Newline);

  while (lexer.token !== Token.CloseParenthesis) {
    const start = lexer.start;
    const name = currentText(lexer);
    const nameRange = currentRange(lexer);
    if (!expect(lexer, Token.Identifier)) return null;
    const isKey = eat(lexer, Token.Colon);
    const isRef = eat(lexer, Token.Ampersand);
    const type = parseType(lexer);
    if (type === null) return null;
    args.push({range: spanSince(lexer, start), name, nameRange, isKey, isRef, type});
    if (!eat(lexer, Token.Comma)) break;
    eat(lexer, Token.Newline);
  }

  eat(lexer, Token.Newline);
  if (!expect(lexer, Token.CloseParenthesis)) return null;
  return args;
}

function parseStmts(lexer: Lexer): Stmt[] | null {
  const stmts: Stmt[] = [];
  if (!expect(lexer, Token.OpenBrace)) return null;
  eat(lexer, Token.Newline);

  while (lexer.token! !== Token.CloseBrace) {
    const start = lexer.start;

    switch (lexer.token!) {
      case Token.Var: {
        advance(lexer);
        const name = currentText(lexer);
        if (!expect(lexer, Token.Identifier)) return null;
        const type: TypeExpr | null = lexer.token === Token.Equals
          ? {range: currentRange(lexer), kind: {kind: 'Inferred'}}
          : parseType(lexer);
        if (type === null || !expect(lexer, Token.Equals)) return null;
        const value = parseExpr(lexer, LEVEL_LOWEST);
        if (value === null) return null;
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'Var', name, type, value}});
        break;
      }

      case Token.Return: {
        advance(lexer);
        if (lexer.token === Token.Newline || lexer.token === Token.CloseBrace) {
          stmts.push({range: spanSince(lexer, start), kind: {kind: 'Return', value: null}});
        } else {
          const value = parseExpr(lexer, LEVEL_LOWEST);
          if (value === null) return null;
          stmts.push({range: spanSince(lexer, start), kind: {kind: 'Return', value}});
        }
        break;
      }

      case Token.While: {
        advance(lexer);
        const test = parseExpr(lexer, LEVEL_LOWEST);
        if (test === null) return null;
        const body = parseStmts(lexer);
        if (body === null) return null;
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'While', test, body}});
        break;
      }

      case Token.If: {
        advance(lexer);
        const test = parseExpr(lexer, LEVEL_LOWEST);
        if (test === null) return null;
        const yes = parseStmts(lexer);
        if (yes === null) return null;
        const no = eat(lexer, Token.Else) ? parseStmts(lexer) : [];
        if (no === null) return null;
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'If', test, yes, no}});
        break;
      }

      default: {
        const value = parseExpr(lexer, LEVEL_LOWEST);
        if (value === null) return null;
        if (eat(lexer, Token.Equals)) {
          const right = parseExpr(lexer, LEVEL_LOWEST);
          if (right === null) return null;
          stmts.push({range: spanSince(lexer, start), kind: {kind: 'Assign', target: value, value: right}});
        } else {
          stmts.push({range: spanSince(lexer, start), kind: {kind: 'Expr', value}});
        }
        break;
      }
    }

    // Recover if there's a semicolon after a statement
    if (lexer.token === Token.Semicolon) {
      unexpected(lexer);
      advance(lexer);
    }

    if (!eat(lexer, Token.Newline)) break;
  }

  if (!expect(lexer, Token.CloseBrace)) return null;
  return stmts;
}

function parseParams(lexer: Lexer): ParamDecl[] | null {
  if (!eat(lexer, Token.LessThan)) {
    return [];
  }

  const params: ParamDecl[] = [];
  eat(lexer, Token.Newline);

  while (lexer.token !== Token.GreaterThan) {
    const start = lexer.start;
    const name = currentText(lexer);
    if (!expect(lexer, Token.Identifier)) return null;
    params.push({range: spanSince(lexer, start), name});
    if (!eat(lexer, Token.Comma)) break;
    eat(lexer, Token.Newline);
  }

  eat(lexer, Token.Newline);
  if (!expect(lexer, Token.GreaterThan)) return null;
  return params;
}

export function parse(log: Log, text: string): Parsed | null {
  const lexer = createLexer(log, text);
  const vars: VarDecl[] = [];
  const defs: DefDecl[] = [];
  const types: TypeDecl[] = [];

  while (lexer.token !== Token.EndOfFile) {
    const start = lexer.start;

    switch (lexer.token!) {
      case Token.Newline:
        advance(lexer);
        break;

      case Token.Var: {
        advance(lexer);
        const name = currentText(lexer);
        const nameRange = currentRange(lexer);
        if (!expect(lexer, Token.Identifier)) return null;
        const type: TypeExpr | null = lexer.token === Token.Equals
          ? {range: currentRange(lexer), kind: {kind: 'Inferred'}}
          : parseType(lexer);
        if (type === null || !expect(lexer, Token.Equals)) return null;
        const value = parseExpr(lexer, LEVEL_LOWEST);
        if (value === null) return null;
        vars.push({range: spanSince(lexer, start), name, nameRange, type, value});
        break;
      }

      case Token.Def: {
        advance(lexer);
        const name = currentText(lexer);
        const nameRange = currentRange(lexer);
        if (!expect(lexer, Token.Identifier)) return null;
        const params = parseParams(lexer);
        if (params === null) return null;
        const args = parseArgs(lexer);
        if (args === null) return null;

        // Recover if there's a colon before the return type
        if (lexer.token === Token.Colon) {
          unexpected(lexer);
          advance(lexer);
        }

        const ret: TypeExpr | null = lexer.token === Token.OpenBrace
          ? {range: currentRange(lexer), kind: {kind: 'Void'}}
          : parseType(lexer);
        if (ret === null) return null;
        const body = parseStmts(lexer);
        if (body === null) return null;
        defs.push({range: spanSince(lexer, start), name, nameRange, params, args, ret, body});
        break;
      }

      case Token.Identifier: {
        if (currentText(lexer) !== 'type') {
          unexpected(lexer);
          return null;
        }

        advance(lexer);
        const ctors: CtorDecl[] = [];
        const name = currentText(lexer);
        const nameRange = currentRange(lexer);
        if (!expect(lexer, Token.Identifier)) return null;
        const params = parseParams(lexer);
        if (params === null) return null;

        // Block form
        if (eat(lexer, Token.OpenBrace)) {
          eat(lexer, Token.Newline);
          while (lexer.token !== Token.CloseBrace) {
            const start = lexer.start;
            const name = currentText(lexer);
            const nameRange = currentRange(lexer);
            if (!expect(lexer, Token.Identifier)) return null;
            const args: ArgDecl[] | null = lexer.token !== Token.OpenParenthesis ? [] : parseArgs(lexer);
            if (args === null) return null;
            ctors.push({range: spanSince(lexer, start), name, nameRange, args});

            // Recover if there's a semicolon or comma after a constructor
            if (lexer.token === Token.Semicolon || lexer.token === Token.Comma) {
              unexpected(lexer);
              advance(lexer);
            }

            if (!eat(lexer, Token.Newline)) break;
          }
          if (!expect(lexer, Token.CloseBrace)) return null;
        }

        // Simple form
        else {
          const args: ArgDecl[] | null = lexer.token === Token.Newline ? [] : parseArgs(lexer);
          if (args === null) return null;
          ctors.push({range: spanSince(lexer, start), name, nameRange, args});
        }

        types.push({range: spanSince(lexer, start), name, nameRange, params, ctors});
        break;
      }

      default: {
        unexpected(lexer);
        return null;
      }
    }
  }

  return {vars, defs, types};
}