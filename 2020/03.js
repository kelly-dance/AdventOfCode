import { product, range, readFile } from '../tools.ts';

const input = readFile('./inputs/03.txt').split('\n').map(l => l.split('').map(c => c === '#'));

const solveSlope = ([xS, yS]) => 
  range(yS, input.length, yS)
    .filter(y => input[y][(y * xS / yS) % input[y].length])
    .length

console.log(solveSlope([3, 1]))

const slopes = [
  [1,1],
  [3,1],
  [5,1],
  [7,1],
  [1,2],
];

console.log(product(slopes.map(solveSlope)))

