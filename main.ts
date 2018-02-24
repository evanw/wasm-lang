declare function require(name: string): any;

interface Range {
  start: number;
  end: number;
}

interface Message {
  range: Range;
  text: string;
}

interface Log {
  messages: Message[];
}

function appendToLog(log: Log, range: Range, text: string): void {
  log.messages.push({range, text});
}

function logToString(source: string, log: Log): string {
  let text = '';
  for (const message of log.messages) {
    const lineStart = source.lastIndexOf('\n', message.range.start - 1) + 1;
    const columnNumber = message.range.start - lineStart + 1;
    let lineEnd = source.indexOf('\n', lineStart);
    if (lineEnd === -1) lineEnd = source.length;
    const line = source.slice(lineStart, lineEnd);
    const lineNumber = source.slice(0, lineStart).split('\n').length;
    const indent = source.slice(0, columnNumber - 1).replace(/[\s\S]/g, ' ');
    const squiggleLength = Math.min(lineEnd, message.range.end) - message.range.start;
    const squiggle = squiggleLength < 2 ? '^' : source.slice(0, squiggleLength).replace(/[\s\S]/g, '~');
    text += `error:${lineNumber}:${columnNumber}: ${message.text}\n${line}\n${indent}${squiggle}\n`;
  }
  return text;
}

////////////////////////////////////////////////////////////////////////////////

enum Token {
  SyntaxError,

  // Whitespace
  EndOfFile,
  Newline,

  // Literals
  Character,
  Identifier,
  Integer,
  String,

  // Punctuation
  Ampersand,
  AmpersandAmpersand,
  Asterisk,
  Bar,
  BarBar,
  CloseBrace,
  CloseBracket,
  CloseParenthesis,
  Colon,
  Comma,
  Dot,
  Equals,
  EqualsEquals,
  ExclamationMark,
  ExclamationMarkEquals,
  GreaterThan,
  GreaterThanEquals,
  LessThan,
  LessThanEquals,
  Minus,
  OpenBrace,
  OpenBracket,
  OpenParenthesis,
  Plus,
  QuestionMark,
  Semicolon,
  Slash,

  // Keywords
  Break,
  Continue,
  Def,
  Else,
  False,
  If,
  Return,
  True,
  Var,
  While,
}

interface Lexer {
  log: Log;
  text: string;
  token: Token;
  start: number;
  end: number;
  previousEnd: number;
}

function createLexer(log: Log, text: string): Lexer {
  const lexer: Lexer = {log, text, token: Token.SyntaxError, start: 0, end: 0, previousEnd: 0};
  advance(lexer);
  return lexer;
}

function currentRange(lexer: Lexer): Range {
  return {start: lexer.start, end: lexer.end};
}

function currentText(lexer: Lexer): string {
  return lexer.text.slice(lexer.start, lexer.end);
}

function isAlpha(c: string): boolean {
  return c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_';
}

function isNumber(c: string): boolean {
  return c >= '0' && c <= '9';
}

function isAlphaOrNumber(c: string): boolean {
  return isAlpha(c) || isNumber(c);
}

function spanSince(lexer: Lexer, start: number): Range {
  return {start, end: lexer.previousEnd};
}

function advance(lexer: Lexer): void {
  const limit = lexer.text.length;
  lexer.previousEnd = lexer.end;
  lexer.start = lexer.end;

  while (lexer.end < limit) {
    const c = lexer.text[lexer.end];
    lexer.end++;

    switch (c) {
      case ' ':
      case '\t':
        lexer.start = lexer.end;
        continue;

      case '#':
        while (lexer.end < limit && lexer.text[lexer.end] !== '\n') {
          lexer.end++;
        }
        lexer.start = lexer.end;
        continue;

      case '\r':
      case '\n':
        // Ignore multiple newlines in a row
        if (lexer.token === Token.Newline) {
          lexer.start = lexer.end;
          continue;
        }

        lexer.token = Token.Newline;
        break;

      case '-': lexer.token = Token.Minus; break;
      case ',': lexer.token = Token.Comma; break;
      case ';': lexer.token = Token.Semicolon; break;
      case ':': lexer.token = Token.Colon; break;
      case '?': lexer.token = Token.QuestionMark; break;
      case '.': lexer.token = Token.Dot; break;
      case '(': lexer.token = Token.OpenParenthesis; break;
      case ')': lexer.token = Token.CloseParenthesis; break;
      case '[': lexer.token = Token.OpenBracket; break;
      case ']': lexer.token = Token.CloseBracket; break;
      case '{': lexer.token = Token.OpenBrace; break;
      case '}': lexer.token = Token.CloseBrace; break;
      case '*': lexer.token = Token.Asterisk; break;
      case '/': lexer.token = Token.Slash; break;
      case '+': lexer.token = Token.Plus; break;

      case '<':
        if (lexer.end < limit && lexer.text[lexer.end] === '=') {
          lexer.end++;
          lexer.token = Token.LessThanEquals;
        } else {
          lexer.token = Token.LessThan;
        }
        break;

        case '>':
        if (lexer.end < limit && lexer.text[lexer.end] === '=') {
          lexer.end++;
          lexer.token = Token.GreaterThanEquals;
        } else {
          lexer.token = Token.GreaterThan;
        }
        break;

      case '=':
        if (lexer.end < limit && lexer.text[lexer.end] === '=') {
          lexer.end++;
          lexer.token = Token.EqualsEquals;
        } else {
          lexer.token = Token.Equals;
        }
        break;

      case '!':
        if (lexer.end < limit && lexer.text[lexer.end] === '=') {
          lexer.end++;
          lexer.token = Token.ExclamationMarkEquals;
        } else {
          lexer.token = Token.ExclamationMark;
        }
        break;

      case '&':
        if (lexer.end < limit && lexer.text[lexer.end] === '&') {
          lexer.end++;
          lexer.token = Token.AmpersandAmpersand;
        } else {
          lexer.token = Token.Ampersand;
        }
        break;

      case '|':
        if (lexer.end < limit && lexer.text[lexer.end] === '|') {
          lexer.end++;
          lexer.token = Token.BarBar;
        } else {
          lexer.token = Token.Bar;
        }
        break;

      case '\'':
      case '"': {
        while (lexer.end < limit) {
          let c2 = lexer.text[lexer.end];
          lexer.end++;

          if (c2 === c) {
            lexer.token = c === '\'' ? Token.Character : Token.String;
            return;
          }

          if (c2 === '\\' && lexer.end < limit) {
            lexer.end++;
          }
        }

        appendToLog(lexer.log, currentRange(lexer), `Syntax error: ${JSON.stringify(currentText(lexer))}`);
        lexer.token = Token.SyntaxError;
        break;
      }

      default:
        if (isAlpha(c)) {
          lexer.token = Token.Identifier;
          while (lexer.end < limit && isAlphaOrNumber(lexer.text[lexer.end])) {
            lexer.end++;
          }
          switch (currentText(lexer)) {
            case 'break': lexer.token = Token.Break; break;
            case 'continue': lexer.token = Token.Continue; break;
            case 'def': lexer.token = Token.Def; break;
            case 'else': lexer.token = Token.Else; break;
            case 'false': lexer.token = Token.False; break;
            case 'if': lexer.token = Token.If; break;
            case 'return': lexer.token = Token.Return; break;
            case 'true': lexer.token = Token.True; break;
            case 'var': lexer.token = Token.Var; break;
            case 'while': lexer.token = Token.While; break;
          }
        } else if (isNumber(c)) {
          lexer.token = Token.Integer;
          while (lexer.end < limit && isNumber(lexer.text[lexer.end])) {
            lexer.end++;
          }
        } else {
          appendToLog(lexer.log, currentRange(lexer), `Syntax error: ${JSON.stringify(currentText(lexer))}`);
          lexer.token = Token.SyntaxError;
        }
        break;
    }

    return;
  }

  lexer.token = Token.EndOfFile;
}

////////////////////////////////////////////////////////////////////////////////

interface Module {
  vars: VarDecl[];
  defs: DefDecl[];
  types: TypeDecl[];
}

interface VarDecl {
  range: Range;
  name: string;
  nameRange: Range;
  type: Type;
  value: Expr;
}

interface DefDecl {
  range: Range;
  name: string;
  nameRange: Range;
  params: Param[];
  args: Arg[];
  ret: Type;
  body: Stmt[]
}

interface TypeDecl {
  range: Range;
  name: string;
  nameRange: Range;
  params: Param[];
  ctors: Ctor[]
}

interface Param {
  range: Range;
  name: string;
}

interface Ctor {
  range: Range;
  name: string;
  nameRange: Range;
  args: Arg[];
}

interface Arg {
  range: Range;
  name: string;
  nameRange: Range;
  isColon: boolean;
  isRef: boolean;
  type: Type;
}

interface Stmt {
  range: Range;
  kind: StmtKind;
}

type StmtKind =
  {kind: 'Var', name: string, type: Type, value: Expr} |
  {kind: 'Return', value: Expr | null} |
  {kind: 'If', test: Expr, yes: Stmt[], no: Stmt[]} |
  {kind: 'While', test: Expr, body: Stmt[]} |
  {kind: 'Assign', target: Expr, value: Expr} |
  {kind: 'Expr', value: Expr};

interface Expr {
  range: Range;
  kind: ExprKind;
}

enum UnOp {
  Not,
  Neg,
}

enum BinOp {
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

type ExprKind =
  {kind: 'Bool', value: boolean} |
  {kind: 'Int', value: number} |
  {kind: 'Name', value: string} |
  {kind: 'String', value: string} |
  {kind: 'Ref', value: Expr} |
  {kind: 'Key', name: string, value: Expr} |
  {kind: 'Array', values: Expr[]} |
  {kind: 'Unary', op: UnOp, value: Expr} |
  {kind: 'Binary', op: BinOp, left: Expr, right: Expr} |
  {kind: 'Call', target: Expr, args: Expr[]} |
  {kind: 'Dot', target: Expr, name: string} |
  {kind: 'Index', target: Expr, value: Expr};

interface Type {
  range: Range;
  kind: TypeKind;
}

type TypeKind =
  {kind: 'Void'} |
  {kind: 'Inferred'} |
  {kind: 'Array', type: Type} |
  {kind: 'Option', type: Type} |
  {kind: 'Name', name: string} |
  {kind: 'Generic', name: string, params: Type[]};

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

function parseTypeSuffix(lexer: Lexer, type: Type): Type | null {
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

function parseType(lexer: Lexer): Type | null {
  const start = lexer.start;
  const name = currentText(lexer);
  if (!expect(lexer, Token.Identifier)) return null;
  if (!eat(lexer, Token.LessThan)) {
    return parseTypeSuffix(lexer, {range: spanSince(lexer, start), kind: {kind: 'Name', name}});
  }

  const params: Type[] = [];
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
            const start2 = lexer.start;
            const isRef2 = eat(lexer, Token.Ampersand);
            value = parseExpr(lexer, LEVEL_LOWEST);
            if (value === null) return null;
            if (isRef2) value = {range: spanSince(lexer, start2), kind: {kind: 'Ref', value}};
            value = {range: spanSince(lexer, start), kind: {kind: 'Key', name, value}};
          }
          args.push(value);
          if (!eat(lexer, Token.Comma)) break;
          eat(lexer, Token.Newline);
        }

        eat(lexer, Token.Newline);
        if (!expect(lexer, Token.CloseParenthesis)) return null;
        left = {range: spanSince(lexer, start), kind: {kind: 'Call', target: left, args}};
        break;
      }

      default:
        return left;
    }
  }
}

function parseArgs(lexer: Lexer): Arg[] | null {
  const args: Arg[] = [];
  if (!expect(lexer, Token.OpenParenthesis)) return null;
  eat(lexer, Token.Newline);

  while (lexer.token !== Token.CloseParenthesis) {
    const start = lexer.start;
    const name = currentText(lexer);
    const nameRange = currentRange(lexer);
    if (!expect(lexer, Token.Identifier)) return null;
    const isColon = eat(lexer, Token.Colon);
    const isRef = eat(lexer, Token.Ampersand);
    const type = parseType(lexer);
    if (type === null) return null;
    args.push({range: spanSince(lexer, start), name, nameRange, isColon, isRef, type});
    if (!eat(lexer, Token.Comma)) break;
    eat(lexer, Token.Newline);
  }

  eat(lexer, Token.Newline);
  if (!expect(lexer, Token.CloseParenthesis)) return null;
  return args;
}

function parseBlock(lexer: Lexer): Stmt[] | null {
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
        const type: Type | null = lexer.token === Token.Equals
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
        const body = parseBlock(lexer);
        if (body === null) return null;
        stmts.push({range: spanSince(lexer, start), kind: {kind: 'While', test, body}});
        break;
      }

      case Token.If: {
        advance(lexer);
        const test = parseExpr(lexer, LEVEL_LOWEST);
        if (test === null) return null;
        const yes = parseBlock(lexer);
        if (yes === null) return null;
        const no = eat(lexer, Token.Else) ? parseBlock(lexer) : [];
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

function parseParams(lexer: Lexer): Param[] | null {
  if (!eat(lexer, Token.LessThan)) {
    return [];
  }

  const params: Param[] = [];
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

function parse(log: Log, text: string): Module | null {
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
        const type: Type | null = lexer.token === Token.Equals
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

        const ret: Type | null = lexer.token === Token.OpenBrace
          ? {range: currentRange(lexer), kind: {kind: 'Void'}}
          : parseType(lexer);
        if (ret === null) return null;
        const body = parseBlock(lexer);
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
        const ctors: Ctor[] = [];
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
            const args: Arg[] | null = lexer.token !== Token.OpenParenthesis ? [] : parseArgs(lexer);
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
          const args: Arg[] | null = lexer.token === Token.Newline ? [] : parseArgs(lexer);
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

////////////////////////////////////////////////////////////////////////////////

interface InsRef {
  // If this is negative, this is the constant stored at constants[~index].
  // Otherwise this is the index of a previous instruction in this block.
  index: number;
}

type Ins =
  {kind: 'Nop'} |
  {kind: 'Alias', value: InsRef} |
  {kind: 'Call', index: number, args: InsRef[]} |

  {kind: 'PtrGlobal', index: number} |
  {kind: 'PtrStack', index: number} |

  {kind: 'MemNew', size: InsRef} |
  {kind: 'MemDelete', ptr: InsRef, size: InsRef} |
  {kind: 'MemCopy', from: InsRef, to: InsRef, size: number, align: number} |

  {kind: 'MemGet8', ptr: InsRef, offset: number} |
  {kind: 'MemSet8', ptr: InsRef, offset: number, value: InsRef} |
  {kind: 'MemGet32', ptr: InsRef, offset: number} |
  {kind: 'MemSet32', ptr: InsRef, offset: number, value: InsRef} |

  {kind: 'LocalGet', local: number} |
  {kind: 'LocalSet', local: number, value: InsRef} |

  {kind: 'Retain', ptr: InsRef} |
  {kind: 'Release', ptr: InsRef, dtor: number} |

  {kind: 'Eq32', left: InsRef, right: InsRef} |
  {kind: 'NotEq32', left: InsRef, right: InsRef} |
  {kind: 'Lt32S', left: InsRef, right: InsRef} |
  {kind: 'Lt32U', left: InsRef, right: InsRef} |
  {kind: 'LtEq32S', left: InsRef, right: InsRef} |
  {kind: 'LtEq32U', left: InsRef, right: InsRef} |

  {kind: 'Add32', left: InsRef, right: InsRef} |
  {kind: 'Sub32', left: InsRef, right: InsRef} |
  {kind: 'Mul32', left: InsRef, right: InsRef} |
  {kind: 'Div32S', left: InsRef, right: InsRef} |
  {kind: 'Div32U', left: InsRef, right: InsRef};

type Jump =
  {kind: 'Goto', target: number} |
  {kind: 'Return', value: InsRef} |
  {kind: 'Branch', value: InsRef, yes: number, no: number};

interface BasicBlock {
  values: Ins[];
  jump: Jump;

  // This map is updated every time a local is loaded or stored in this block.
  // It's used to optimize away "LocalGet" calls if a local was already loaded.
  previousLocals: {[index: number]: InsRef};
}

interface Graph {
  blocks: BasicBlock[];

  // This stores any constants indexed by "InsRef"
  constants: number[];

  // This stores the byte size of each local (used by the "LocalGet" and "LocalSet" instructions)
  locals: number[];

  // This stores the byte size of each stack slot (used by the "PtrStack" instruction)
  stack: number[];
}

function createGraph(): Graph {
  return {
    blocks: [],
    constants: [0],
    locals: [],
    stack: [],
  };
}

function createBlock(graph: Graph): number {
  graph.blocks.push({
    values: [],
    jump: {kind: 'Return', value: {index: ~0}},
    previousLocals: {},
  });
  return graph.blocks.length - 1;
}

interface ValueRef {
  block: number;
  ref: InsRef;
}

function unwrap(graph: Graph, block: number, value: ValueRef): InsRef {
  // TODO: Generate a "LocalGet" if needed
}

function addIns(graph: Graph, ins: Ins): ValueRef {
  // TODO
}

////////////////////////////////////////////////////////////////////////////////

interface Field {
  name: string;
  typeID: number;
}

interface Variant {
  name: string;
  fields: Field[];
}

interface TypeData {
  name: string;
  variants: Variant[];
  defaultVariant: number | null;
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

interface Code {
  types: TypeData[];
  defs: DefData[];
  globalScope: {[name: string]: GlobalRef};
  errorTypeID: number;
}

function defineGlobal(log: Log, code: Code, range: Range, name: string, ref: GlobalRef): boolean {
  if (name in code.globalScope) {
    appendToLog(log, range, `The name "${name}" is already used`);
    return false;
  }

  code.globalScope[name] = ref;
  return true;
}

function addTypeID(log: Log, code: Code, range: Range, name: string): number {
  const typeID = code.types.length;
  if (!defineGlobal(log, code, range, name, {kind: 'Type', typeID})) {
    return code.errorTypeID;
  }

  code.types.push({name, variants: [], defaultVariant: null});
  return typeID;
}

function resolveToTypeID(log: Log, code: Code, type: Type): number {
  if (type.kind.kind === 'Name') {
    const name = type.kind.name;
    const ref = code.globalScope[name];

    if (!ref) {
      appendToLog(log, type.range, `There is no type named "${name}"`);
      return code.errorTypeID;
    }

    if (ref.kind === 'Ctor') {
      const typeName = code.types[ref.typeID].name;
      appendToLog(log, type.range, `Use the type "${typeName}" instead of the constructor "${name}"`);
      return code.errorTypeID;
    }

    if (ref.kind !== 'Type') {
      appendToLog(log, type.range, `The name "${name}" is not a type`);
      return code.errorTypeID;
    }

    return ref.typeID;
  }

  else {
    appendToLog(log, type.range, `Unsupported type of kind ${type.kind.kind}`);
  }

  return code.errorTypeID;
}

function compileTypes(log: Log, module: Module, code: Code): void {
  // Add type names first
  for (const type of module.types) {
    if (type.params.length === 0) {
      const typeID = addTypeID(log, code, type.nameRange, type.name);
      const data = code.types[typeID];

      // Also add variants
      for (const ctor of type.ctors) {
        const index = data.variants.length;

        // Allow one variant to share the name of the type
        if (ctor.name === type.name && data.defaultVariant === null) {
          data.defaultVariant = data.variants.length;
          data.variants.push({name: ctor.name, fields: []});
        }

        // Otherwise the variant must have a unique global name
        else if (defineGlobal(log, code, ctor.nameRange, ctor.name, {kind: 'Ctor', typeID, index})) {
          data.variants.push({name: ctor.name, fields: []});
        }
      }
    }
  }

  // Add fields next
  for (const type of module.types) {
    if (type.params.length === 0) {
      let foundDefault = false;

      // Turn each constructor into a variant
      for (const ctor of type.ctors) {
        let ref = code.globalScope[ctor.name];

        // Allow one variant to share the name of the type
        if (ref.kind === 'Type' && ctor.name === type.name && !foundDefault) {
          const data = code.types[ref.typeID];
          if (data.defaultVariant !== null) {
            foundDefault = true;
            ref = {kind: 'Ctor', typeID: ref.typeID, index: data.defaultVariant};
          }
        }

        // The ref can be null if there are two variants with the type's name (only the first one is the default)
        if (!ref || ref.kind !== 'Ctor') continue;
        const fields = code.types[ref.typeID].variants[ref.index].fields;

        // Turn constructor arguments into fields
        for (const arg of ctor.args) {
          const typeID = resolveToTypeID(log, code, arg.type);
          fields.push({name: arg.name, typeID});
        }
      }
    }
  }
}

interface DefBuilder {
}

interface Scope {
  parent: Scope;
  locals: {[name: string]: number};
}

function compileDefs(log: Log, module: Module, code: Code): void {
  // Resolve all argument types and return types
  for (const def of module.defs) {
    if (defineGlobal(log, code, def.nameRange, def.name, {kind: 'Def', defID: code.defs.length})) {
      const argTypeIDs: number[] = [];
      for (const arg of def.args) {
        argTypeIDs.push(resolveToTypeID(log, code, arg.type));
      }
      const retTypeID = resolveToTypeID(log, code, def.ret);
      code.defs.push({name: def.name, argTypeIDs, retTypeID, graph: null});
    }
  }
}

function compile(log: Log, module: Module): Code {
  const code: Code = {
    types: [],
    defs: [],
    globalScope: {},
    errorTypeID: 0,
  };

  code.errorTypeID = addTypeID(log, code, {start: 0, end: 0}, '(error)');
  compileTypes(log, module, code);
  compileDefs(log, module, code);
  return code;
}

////////////////////////////////////////////////////////////////////////////////

export function main(): void {
  const fs = require('fs');
  const source = fs.readFileSync('example.txt', 'utf8');
  const log: Log = {messages: []};
  const module = parse(log, source);
  const code = module && compile(log, module);

  console.log(logToString(source, log));
  if (log.messages.length === 0) {
    // console.log(require('util').inspect(module, {depth: Infinity}));
  }
  console.log('done');
}

main();
