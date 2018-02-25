export interface Range {
  start: number;
  end: number;
}

export interface Message {
  range: Range;
  text: string;
}

export interface Log {
  messages: Message[];
}

export function appendToLog(log: Log, range: Range, text: string): void {
  log.messages.push({range, text});
}

export function logToString(source: string, log: Log): string {
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
