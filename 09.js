import { combos, filterGenerator, first, range, readFile, sum, subSequences, iteratorSome } from './tools.ts';

const input = readFile('inputs/09.txt').split('\r\n').map(Number);

const p1 = input[first(filterGenerator(
  () => range(25, input.length)[Symbol.iterator](),
  idx => !iteratorSome(
    combos(input.slice(idx - 25, idx), 2),
    ns => sum(ns) === input[idx]
  )
)())];

console.log(p1);

const p2Sqn = first(filterGenerator(
  subSequences(input, 2),
  ns => sum(ns) === p1
)());

console.log(Math.min(...p2Sqn) + Math.max(...p2Sqn))
