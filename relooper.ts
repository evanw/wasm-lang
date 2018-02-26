// This file implements emscripten's relooper algorithm:
// https://github.com/kripken/Relooper/blob/master/paper.pdf

import { Code, RawType, Func, blockToString, refToString } from './ssa';
import { assert } from './util';

type BlockTree =
  {kind: 'Simple', block: number, next: BlockTree | null} |
  {kind: 'Loop', body: BlockTree, next: BlockTree | null} |
  {kind: 'Multiple', branches: [number, BlockTree][], next: BlockTree | null};

interface BlockData {
  isLoop: boolean;
  incoming: number[];
  outgoing: number[];
}

export function blocksToTree(func: Func): BlockTree {
  const live = new Set<number>();
  const blocks: BlockData[] = [];

  // Discover outgoing edges
  for (const block of func.blocks) {
    let outgoing: number[] = [];
    switch (block.jump.kind) {
      case 'Goto': outgoing.push(block.jump.target); break;
      case 'Branch': outgoing.push(block.jump.yes, block.jump.no); break;
    }
    blocks.push({isLoop: false, incoming: [], outgoing});
  }

  // Discover incoming edges
  for (let i = 0; i < blocks.length; i++) {
    for (const edge of blocks[i].outgoing) {
      blocks[edge].incoming.push(i);
    }
  }

  // Consider everything connected to the entry point
  markLiveDFS(blocks, live, 0);
  return handleSingleEntry(blocks, 0, live);
}

function markLiveDFS(blocks: BlockData[], live: Set<number>, index: number): void {
  if (!live.has(index)) {
    live.add(index);
    for (const edge of blocks[index].outgoing) {
      markLiveDFS(blocks, live, edge);
    }
  }
}

function handleMultipleEntries(blocks: BlockData[], entries: number[], live: Set<number>): BlockTree {
  assert(entries.length > 0);
  assert(entries.every(entry => live.has(entry)));

  // Special-case a single entry point
  if (entries.length === 1) {
    return handleSingleEntry(blocks, entries[0], live);
  }

  // If there wasn't a single entry, partition the live blocks into those only
  // reachable from a single entry point and everything else
  const owners = new Map<number, number | null>();
  for (const entry of entries) {
    markOwnerDFS(entry, blocks, live, owners, entry);
  }

  // Extract the groups from the owners
  const nextLive = new Set<number>();
  const groups = new Map<number, Set<number>>();
  for (const [index, entry] of owners) {
    if (entry === null) {
      nextLive.add(index);
    } else {
      const group = groups.get(entry);
      if (group !== undefined) group.add(index);
      else groups.set(entry, new Set([index]));
    }
  }

  // The next blocks are all blocks not in any group
  const nextEntries = new Set<number>();
  for (const index of nextLive) {
    for (const edge of blocks[index].incoming) {
      if (!nextLive.has(edge) && live.has(edge)) {
        nextEntries.add(index);
      }
    }
  }

  const branches: [number, BlockTree][] = [];
  for (const [entry, group] of groups) {
    branches.push([entry, handleSingleEntry(blocks, entry, group)]);
  }
  const next = nextEntries.size > 0 ? handleMultipleEntries(blocks, [...nextEntries], nextLive) : null;
  return {kind: 'Multiple', branches, next};
}

function handleSingleEntry(blocks: BlockData[], entry: number, live: Set<number>): BlockTree {
  assert(live.has(entry));

  // Find all live blocks that loop back to this block
  const backEdgeBlocks = new Set<number>();
  const block = blocks[entry];
  if (!block.isLoop) {
    for (const edge of block.incoming) {
      if (live.has(edge)) {
        backEdgeBlocks.add(edge);
      }
    }
  }

  // Make a simple block if this isn't a loop
  if (backEdgeBlocks.size === 0) {
    const nextEntries = new Set<number>();
    live.delete(entry);
    for (const edge of block.outgoing) {
      if (live.has(edge)) {
        nextEntries.add(edge);
      }
    }
    const next = nextEntries.size > 0 ? handleMultipleEntries(blocks, [...nextEntries], live) : null;
    return {kind: 'Simple', block: entry, next};
  }

  // Otherwise, we need to make a loop
  const bodyLive = new Set<number>();
  block.isLoop = true;

  // Find the loop body by finding all blocks reachable via a backward search
  // starting at the back edges and working towards the entry
  const worklist: number[] = [];
  for (const index of backEdgeBlocks) {
    worklist.push(index);
  }
  while (worklist.length > 0) {
    const index = worklist.pop()!;
    if (live.has(index) && !bodyLive.has(index)) {
      bodyLive.add(index);
      for (const edge of blocks[index].incoming) {
        worklist.push(edge);
      }
    }
  }

  // The next blocks are all blocks not in the body
  const nextLive = new Set<number>();
  const nextEntries = new Set<number>();
  for (const index of live) {
    if (!bodyLive.has(index)) {
      nextLive.add(index);
      if (blocks[index].incoming.some(i => bodyLive.has(i))) {
        nextEntries.add(index);
      }
    }
  }

  const body = handleSingleEntry(blocks, entry, bodyLive);
  const next = nextEntries.size > 0 ? handleMultipleEntries(blocks, [...nextEntries], nextLive) : null;
  return {kind: 'Loop', body, next};
}

function markOwnerDFS(index: number, blocks: BlockData[], live: Set<number>, owners: Map<number, number | null>, owner: number | null): void {
  // Ignore blocks not in the live set
  if (!live.has(index)) {
    return;
  }

  const current = owners.get(index);

  // Take ownership if this is the first time we've seen this block
  if (current === undefined) {
    owners.set(index, owner);
    for (const edge of blocks[index].outgoing) {
      markOwnerDFS(edge, blocks, live, owners, owner);
    }
  }

  // Remove the owner if this block is owned by another entry point
  else if (current !== owner && current !== null) {
    owners.set(index, null);
    for (const edge of blocks[index].outgoing) {
      markOwnerDFS(edge, blocks, live, owners, null);
    }
  }
}

export function codeToTreeString(code: Code): string {
  let text = '';

  for (const func of code.funcs) {
    const args = func.argTypes.map((arg, i) => `v${i} ${RawType[arg]}`).join(', ');
    text += `def ${func.name}(${args}) ${RawType[func.retType]}\n`;

    text += '  locals:\n';
    for (let i = 0; i < func.locals.length; i++) {
      text += `    v${i} ${RawType[func.locals[i]]}\n`;
    }

    text += treeToString(code, func, blocksToTree(func), '  ');
  }

  return text;
}

function treeToString(code: Code, func: Func, tree: BlockTree | null, indent: string): string {
  if (tree === null) return '';
  let text = '';

  switch (tree.kind) {
    case 'Simple': {
      const block = func.blocks[tree.block];
      text += `${indent}# b${tree.block}\n`;
      text += blockToString(code, func, block, indent);
      switch (block.jump.kind) {
        case 'Missing':
          break;

        case 'ReturnVoid':
          text += `${indent}return\n`;
          break;

        case 'Return':
          text += `${indent}return ${refToString(func, block.jump.value)}\n`;
          break;

        case 'Goto':
          text += `${indent}goto b${block.jump.target}\n`;
          break;

        case 'Branch':
          text += `${indent}branch ${refToString(func, block.jump.value)} ? b${block.jump.yes} : b${block.jump.no}\n`;
          break;

        default: {
          const checkCovered: void = block.jump;
          throw new Error('Internal error');
        }
      }
      break;
    }

    case 'Loop': {
      text += `${indent}loop {\n`;
      text += treeToString(code, func, tree.body, indent + '  ');
      text += `${indent}}\n`;
      break;
    }

    case 'Multiple': {
      text += `${indent}multiple {\n`;
      for (const [entry, branch] of tree.branches) {
        text += `${indent}  branch b${entry} {\n`;
        text += treeToString(code, func, branch, indent + '    ');
        text += `${indent}  }\n`;
      }
      text += `${indent}}\n`;
      break;
    }

    default: {
      const checkCovered: void = tree;
      throw new Error('Internal error');
    }
  }

  text += treeToString(code, func, tree.next, indent);
  return text;
}
