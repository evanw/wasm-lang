import {
  Func,
  InsRef,
  getConstant,
  BinIns,
  createConstant,
} from "./ssa";

export function optimize(func: Func): void {
  // Count how many times each local is read from
  const isReadFrom: boolean[] = [];
  for (let i = 0; i < func.locals.length; i++) {
    isReadFrom.push(false);
  }
  for (const block of func.blocks) {
    for (const ins of block.insList) {
      if (ins.kind === 'LocalGet') {
        isReadFrom[ins.local] = true;
      }
    }
  }

  // Do a pass over all instructions
  for (const block of func.blocks) {
    const aliases = new Map<number, InsRef>();
    const mapAlias = (ref: InsRef) => aliases.get(ref.index) || ref;

    for (let i = 0; i < block.insList.length; i++) {
      const ins = block.insList[i];

      switch (ins.kind) {
        case 'Nop':
        case 'PtrGlobal':
        case 'PtrStack':
        case 'LocalGet':
          break;

        case 'Call':
        case 'CallImport':
        case 'CallIntrinsic':
          ins.args = ins.args.map(mapAlias);
          break;

        case 'MemAlloc':
          ins.size = mapAlias(ins.size);
          break;

        case 'MemFree':
          ins.ptr = mapAlias(ins.ptr);
          ins.size = mapAlias(ins.size);
          break;

        case 'MemGet8':
        case 'MemGet32':
          ins.ptr = mapAlias(ins.ptr);
          break;

        case 'MemSet8':
        case 'MemSet32':
          ins.ptr = mapAlias(ins.ptr);
          ins.value = mapAlias(ins.value);
          break;

        case 'LocalSet': {
          if (!isReadFrom[ins.local]) {
            // Remove sets to unread locals
            block.insList[i] = {kind: 'Nop'};
          } else {
            ins.value = mapAlias(ins.value);
          }
          break;
        }

        case 'Retain':
        case 'Release':
          ins.ptr = mapAlias(ins.ptr);
          break;

        case 'Binary': {
          ins.left = mapAlias(ins.left);
          ins.right = mapAlias(ins.right);

          // Fold constants
          const left = getConstant(func, ins.left);
          const right = getConstant(func, ins.right);
          if (left !== null && right !== null) {
            let value: number;

            switch (ins.op) {
              case BinIns.Eq32: value = left === right ? 1 : 0; break;
              case BinIns.NotEq32: value = left !== right ? 1 : 0; break;
              case BinIns.Lt32S: value = left < right ? 1 : 0; break;
              case BinIns.Lt32U: value = (left >>> 0) < (right >>> 0) ? 1 : 0; break;
              case BinIns.LtEq32S: value = left <= right ? 1 : 0; break;
              case BinIns.LtEq32U: value = (left >>> 0) <= (right >>> 0) ? 1 : 0; break;
              case BinIns.And32: value = left & right; break;
              case BinIns.Or32: value = left | right; break;
              case BinIns.Xor32: value = left ^ right; break;
              case BinIns.Add32: value = left + right | 0; break;
              case BinIns.Sub32: value = left - right | 0; break;
              case BinIns.Mul32: value = Math.imul(left, right); break;
              case BinIns.Div32S: value = left / right | 0; break;
              case BinIns.Div32U: value = (left >>> 0) / (right >>> 0) | 0; break;
              case BinIns.Shl32: value = left << right; break;
              case BinIns.Shr32S: value = left >> right; break;
              case BinIns.Shr32U: value = left >>> right; break;

              default: {
                const checkCovered: void = ins.op;
                throw new Error('Internal error');
              }
            }

            aliases.set(i, createConstant(func, value).ref);
            block.insList[i] = {kind: 'Nop'};
          }
          break;
        }

        default: {
          const checkCovered: void = ins;
          throw new Error('Internal error');
        }
      }
    }
  }
}
