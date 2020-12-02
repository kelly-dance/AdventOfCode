import { combos, filterGenerator, product, readFile, sum } from './tools.js';

const nums = readFile('./inputs/01.txt').split('\n').map(Number);

console.log(...[2,3].map(s => product(
  filterGenerator(
    () => combos(nums, s),
    ns => sum(ns) === 2020
  )().next().value
)));
