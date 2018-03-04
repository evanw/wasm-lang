import { Log, Range, appendToLog } from './log';
import { Token, currentText, spanSince, advance, Lexer, currentRange, createLexer } from './lexer';

export interface Parsed {
  sourceNames: string[];
  vars: VarDecl[];
  defs: DefDecl[];
  types: TypeDecl[];
}

export interface VarDecl {
  range: Range;
  name: string;
  nameRange: Range;
  tags: Tag[];
  type: TypeExpr;
  value: Expr;
}

export interface Tag {
  range: Range;
  name: string;
  args: Expr[];
}

export interface DefDecl {
  range: Range;
  name: string;
  nameRange: Range;
  tags: Tag[];
  params: ParamDecl[];
  args: ArgDecl[];
  ret: TypeExpr;
  body: Stmt[] | null;
}

export interface TypeDecl {
  range: Range;
  name: string;
  nameRange: Range;
  tags: Tag[];
  params: ParamDecl[];
  ctors: CtorDecl[];
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
  {kind: 'Var', name: string, nameRange: Range, type: TypeExpr, value: Expr} |
  {kind: 'Return', value: Expr | null} |
  {kind: 'Break', count: number} |
  {kind: 'Continue', count: number} |
  {kind: 'If', test: Expr, yes: Stmt[], no: Stmt[]} |
  {kind: 'While', test: Expr, body: Stmt[]} |
  {kind: 'Assign', target: Expr, value: Expr} |
  {kind: 'Expr', value: Expr};

export interface Expr {
  range: Range;
  kind: ExprKind;
}

export enum UnOp {
  Cpl,
  Neg,
  Not,
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
  BitAnd,
  BitOr,
  BitXor,
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
  {kind: 'Dot', target: Expr, name: string, nameRange: Range} |
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
          default: appendToLog(lexer.log, currentRange(lexer), `Invalid escape sequence in character literal`); break;
        }
      } else {
        appendToLog(lexer.log, currentRange(lexer), `Invalid character literal`);
      }

      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'Int', value}};
    }

    case Token.String: {
      const text = currentText(lexer);
      const limit = text.length - 1;
      let value = '';

      for (let i = 1; i < limit; i++) {
        let c = text[i];
        if (c === '\\') {
          if (i + 1 < limit) {
            i += 1;
            switch (text[i]) {
              case '0': c = '\0'; break;
              case 'b': c = '\b'; break;
              case 'n': c = '\n'; break;
              case 'r': c = '\r'; break;
              case 't': c = '\t'; break;
              default: appendToLog(lexer.log, currentRange(lexer), `Invalid escape sequence in string literal`); break;
            }
          } else {
            appendToLog(lexer.log, currentRange(lexer), `Invalid escape sequence in string literal`);
          }
        }
        value += c;
      }

      advance(lexer);
      return {range: spanSince(lexer, start), kind: {kind: 'String', value}};
    }

    case Token.Tilde: {
      advance(lexer);
      const value = parseExpr(lexer, LEVEL_PREFIX);
      if (value === null) return null;
      return {range: spanSince(lexer, start), kind: {kind: 'Unary', op: UnOp.Cpl, value}};
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

    case Token.Ampersand: {
      advance(lexer);
      const value = parseExpr(lexer, LEVEL_PREFIX);
      if (value === null) return null;
      return {range: spanSince(lexer, start), kind: {kind: 'Ref', value}};
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
const LEVEL_OR = 1;
const LEVEL_AND = 2;
const LEVEL_BIT_OR = 3;
const LEVEL_BIT_XOR = 4;
const LEVEL_BIT_AND = 5;
const LEVEL_EQUALITY = 6;
const LEVEL_COMPARE = 7;
const LEVEL_ADD = 8;
const LEVEL_MULTIPLY = 9;
const LEVEL_PREFIX = 10;

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
        if (level >= LEVEL_OR) return left;
        left = parseBinary(lexer, left, BinOp.Or, LEVEL_OR);
        break;
      }

      case Token.AmpersandAmpersand: {
        if (level >= LEVEL_AND) return left;
        left = parseBinary(lexer, left, BinOp.And, LEVEL_AND);
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

      case Token.Ampersand: {
        if (level >= LEVEL_BIT_AND) return left;
        left = parseBinary(lexer, left, BinOp.BitAnd, LEVEL_BIT_AND);
        break;
      }

      case Token.Bar: {
        if (level >= LEVEL_BIT_OR) return left;
        left = parseBinary(lexer, left, BinOp.BitOr, LEVEL_BIT_OR);
        break;
      }

      case Token.Caret: {
        if (level >= LEVEL_BIT_XOR) return left;
        left = parseBinary(lexer, left, BinOp.BitXor, LEVEL_BIT_XOR);
        break;
      }

      case Token.Dot: {
        advance(lexer);
        const name = currentText(lexer);
        const nameRange = currentRange(lexer);
        if (!expect(lexer, Token.Identifier)) return null;
        left = {range: spanSince(lexer, start), kind: {kind: 'Dot', target: left, name, nameRange}};
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
          const isMaybeKey = lexer.token === Token.Identifier;
          let value = parseExpr(lexer, LEVEL_LOWEST);
          if (value === null) return null;
          else if (isMaybeKey && value.kind.kind === 'Name' && eat(lexer, Token.Colon)) {
            const name = value.kind.value;
            const nameRange = spanSince(lexer, value.range.start);
            value = parseExpr(lexer, LEVEL_LOWEST);
            if (value === null) return null;
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

function parseStmts(lexer: Lexer, enclosingLoopCount: number): Stmt[] | null {
  const stmts: Stmt[] = [];
  if (!expect(lexer, Token.OpenBrace)) return null;
  eat(lexer, Token.Newline);

  while (lexer.token! !== Token.CloseBrace) {
    const start = lexer.start;

    switch (lexer.token!) {
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
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'Var', name, nameRange, type, value}});
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

      case Token.Break: {
        const count = parseJumpWithLoopCount(lexer, enclosingLoopCount, "break");
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'Break', count}});
        break;
      }

      case Token.Continue: {
        const count = parseJumpWithLoopCount(lexer, enclosingLoopCount, "continue");
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'Continue', count}});
        break;
      }

      case Token.While: {
        advance(lexer);
        const test = parseExpr(lexer, LEVEL_LOWEST);
        if (test === null) return null;
        const body = parseStmts(lexer, enclosingLoopCount + 1);
        if (body === null) return null;
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'While', test, body}});
        break;
      }

      case Token.If: {
        advance(lexer);
        const test = parseExpr(lexer, LEVEL_LOWEST);
        if (test === null) return null;
        const yes = parseStmts(lexer, enclosingLoopCount);
        if (yes === null) return null;
        const no = eat(lexer, Token.Else) ? parseStmts(lexer, enclosingLoopCount) : [];
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

function parseJumpWithLoopCount(lexer: Lexer, enclosingLoopCount: number, name: string): number {
  const start = lexer.start;
  let count = 1;
  advance(lexer);

  if (lexer.token === Token.Integer) {
    count = +currentText(lexer);
    advance(lexer);
  }

  if (enclosingLoopCount === 0) {
    appendToLog(lexer.log, spanSince(lexer, start), `Cannot use "${name}" outside of a loop`);
  }

  else if (count < 1 || count > enclosingLoopCount) {
    appendToLog(lexer.log, spanSince(lexer, start), `There is no enclosing loop with index ${count}`);
  }

  return count;
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

export function parse(log: Log, text: string, source: number, parsed: Parsed): boolean {
  const lexer = createLexer(log, text, source);
  let tags: Tag[] = [];

  while (lexer.token! !== Token.EndOfFile) {
    const start = lexer.start;

    switch (lexer.token!) {
      case Token.Newline:
        advance(lexer);
        continue;

      case Token.AtSign: {
        advance(lexer);
        const name = currentText(lexer);
        if (!expect(lexer, Token.Identifier)) return false;
        const args: Expr[] = [];
        if (eat(lexer, Token.OpenParenthesis)) {
          eat(lexer, Token.Newline);

          while (lexer.token !== Token.CloseBracket) {
            const arg = parseExpr(lexer, LEVEL_LOWEST);
            if (arg === null) return false;
            args.push(arg);
            if (!eat(lexer, Token.Comma)) break;
            eat(lexer, Token.Newline);
          }

          eat(lexer, Token.Newline);
          if (!expect(lexer, Token.CloseParenthesis)) return false;
        }
        tags.push({range: spanSince(lexer, start), name, args});
        break;
      }

      case Token.Var: {
        advance(lexer);
        const name = currentText(lexer);
        const nameRange = currentRange(lexer);
        if (!expect(lexer, Token.Identifier)) return false;
        const type: TypeExpr | null = lexer.token === Token.Equals
          ? {range: currentRange(lexer), kind: {kind: 'Inferred'}}
          : parseType(lexer);
        if (type === null || !expect(lexer, Token.Equals)) return false;
        const value = parseExpr(lexer, LEVEL_LOWEST);
        if (value === null) return false;
        parsed.vars.push({range: spanSince(lexer, start), name, nameRange, tags, type, value});
        tags = [];
        break;
      }

      case Token.Def: {
        advance(lexer);
        const name = currentText(lexer);
        const nameRange = currentRange(lexer);
        if (!expect(lexer, Token.Identifier)) return false;
        const params = parseParams(lexer);
        if (params === null) return false;
        const args = parseArgs(lexer);
        if (args === null) return false;

        // Recover if there's a colon before the return type
        if (lexer.token === Token.Colon) {
          unexpected(lexer);
          advance(lexer);
        }

        const ret: TypeExpr | null = lexer.token === Token.OpenBrace
          ? {range: currentRange(lexer), kind: {kind: 'Void'}}
          : parseType(lexer);
        if (ret === null) return false;
        let body: Stmt[] | null = null;
        if (lexer.token === Token.OpenBrace) {
          body = parseStmts(lexer, 0);
          if (body === null) return false;
        }
        parsed.defs.push({range: spanSince(lexer, start), name, nameRange, tags, params, args, ret, body});
        tags = [];
        break;
      }

      case Token.Identifier: {
        if (currentText(lexer) !== 'type') {
          unexpected(lexer);
          return false;
        }

        advance(lexer);
        const ctors: CtorDecl[] = [];
        const name = currentText(lexer);
        const nameRange = currentRange(lexer);
        if (!expect(lexer, Token.Identifier)) return false;
        const params = parseParams(lexer);
        if (params === null) return false;

        // Block form
        if (eat(lexer, Token.OpenBrace)) {
          eat(lexer, Token.Newline);
          while (lexer.token !== Token.CloseBrace) {
            const start = lexer.start;
            const name = currentText(lexer);
            const nameRange = currentRange(lexer);
            if (!expect(lexer, Token.Identifier)) return false;
            const args: ArgDecl[] | null = lexer.token !== Token.OpenParenthesis ? [] : parseArgs(lexer);
            if (args === null) return false;
            ctors.push({range: spanSince(lexer, start), name, nameRange, args});

            // Recover if there's a semicolon or comma after a constructor
            if (lexer.token === Token.Semicolon || lexer.token === Token.Comma) {
              unexpected(lexer);
              advance(lexer);
            }

            if (!eat(lexer, Token.Newline)) break;
          }
          if (!expect(lexer, Token.CloseBrace)) return false;
        }

        // Simple form
        else {
          const args: ArgDecl[] | null = lexer.token === Token.Newline ? [] : parseArgs(lexer);
          if (args === null) return false;
          ctors.push({range: spanSince(lexer, start), name, nameRange, args});
        }

        parsed.types.push({range: spanSince(lexer, start), name, nameRange, tags, params, ctors});
        tags = [];
        break;
      }

      default: {
        unexpected(lexer);
        return false;
      }
    }

    // Declarations must be separated by newlines
    if (lexer.token !== Token.EndOfFile && !expect(lexer, Token.Newline)) {
      return false;
    }
  }

  // If we've parsed some tags, require a declaration after the tags
  if (tags.length !== 0) {
    unexpected(lexer);
    return false;
  }

  return true;
}
