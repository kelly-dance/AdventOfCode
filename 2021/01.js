import { countMatches, readAdvent, zip  } from '../tools.ts';

const depths = (await readAdvent()).split('\n').map(s => Number(s));

console.log(countMatches(zip(depths, depths.slice(1)), ([a, b]) => b > a));
console.log(countMatches(zip(depths, depths.slice(3)), ([a, b]) => b > a));
