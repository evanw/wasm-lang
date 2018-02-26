import { appendToLog, Range, Log } from './log';

export enum Token {
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
  Caret,
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
  Tilde,

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

export interface Lexer {
  log: Log;
  text: string;
  token: Token;
  start: number;
  end: number;
  previousEnd: number;
}

export function createLexer(log: Log, text: string): Lexer {
  const lexer: Lexer = {log, text, token: Token.SyntaxError, start: 0, end: 0, previousEnd: 0};
  advance(lexer);
  return lexer;
}

export function currentRange(lexer: Lexer): Range {
  return {start: lexer.start, end: lexer.end};
}

export function currentText(lexer: Lexer): string {
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

export function spanSince(lexer: Lexer, start: number): Range {
  return {start, end: lexer.previousEnd};
}

export function advance(lexer: Lexer): void {
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
      case '^': lexer.token = Token.Caret; break;
      case '+': lexer.token = Token.Plus; break;
      case '~': lexer.token = Token.Tilde; break;

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
