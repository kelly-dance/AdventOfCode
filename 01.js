import { combos, filterGenerator, takeFirst, product, readFile, sum } from './tools.ts';

const nums = readFile('./inputs/01.txt').split('\n').map(Number);

console.log(...[2,3].map(s => product(
  takeFirst(
    filterGenerator(
      () => combos(nums, s),
      ns => sum(ns) === 2020
    )()
  )
)));
