import { combos, filterGenerator, first, product, readFile, sum } from './tools.ts';

const nums = readFile('./inputs/01.txt').split('\n').map(Number);

console.log(...[2,3].map(s => product(
  first(
    filterGenerator(
      () => combos(nums, s),
      ns => sum(ns) === 2020
    )()
  )
)));
