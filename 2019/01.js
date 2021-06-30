import { readFile, sum } from '../tools.ts';

const fuel = mass => {
  let mine = Math.floor(mass / 3) - 2;
  if(mine < 0) return mass;
  return mass + fuel(mine);
}

const input = readFile('./inputs/01.txt').split('\n').map(Number);

console.log(sum(input.map(mass => Math.floor(mass / 3) - 2)));
console.log(sum(input.map(mass => fuel(mass) - mass)));
