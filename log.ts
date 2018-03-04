export interface Range {
  source: number;
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

export function logToString(log: Log, sourceNames: string[], sources: string[]): string {
  let text = '';

  for (const message of log.messages) {
    const sourceName = sourceNames[message.range.source];
    const source = sources[message.range.source];
    const lineStart = source.lastIndexOf('\n', message.range.start - 1) + 1;
    const column = message.range.start - lineStart + 1;
    let lineEnd = source.indexOf('\n', lineStart);
    if (lineEnd === -1) lineEnd = source.length;
    const lineText = source.slice(lineStart, lineEnd);
    const line = source.slice(0, lineStart).split('\n').length;
    const indent = source.slice(0, column - 1).replace(/[\s\S]/g, ' ');
    const squiggleLength = Math.min(lineEnd, message.range.end) - message.range.start;
    const squiggle = squiggleLength < 2 ? '^' : source.slice(0, squiggleLength).replace(/[\s\S]/g, '~');
    text += `${sourceName}: error:${line}:${column}: ${message.text}\n${lineText}\n${indent}${squiggle}\n`;
  }

  return text;
}
