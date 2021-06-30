import { readFile, sum, pickIntsFromString, product } from '../tools.ts';

const inp = readFile('./inputs/02.txt').split('\n').map(l => pickIntsFromString(l));

console.log(sum(inp.map(([a,b,c]) => {
  const areas = [a*b,b*c,c*a];
  return 2 * sum(areas) + Math.min(...areas);
})))

console.log(sum(inp.map(sides => {
  return product(sides) + 2 * (sum(sides) - Math.max(...sides)) 
})))

