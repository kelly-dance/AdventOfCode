
import { readFile, range, sum } from '../tools.ts';

const input = readFile('./inputs/01.txt').split('').map(c => parseInt(c));
console.log(sum(range(input.length - 1).map(i => input[i] === input[(i + 1) % input.length] ? input[i] : 0)))
console.log(sum(range(input.length - 1).map(i => input[i] === input[(i + Math.floor(input.length / 2)) % input.length] ? input[i] : 0)))
