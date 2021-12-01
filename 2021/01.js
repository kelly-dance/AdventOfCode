import { countMatches, readFile, sum, zip, zipWith  } from '../tools.ts';

const depths = readFile('./inputs/01.txt').split('\n').map(s => Number(s));

console.log(countMatches(zip(depths, depths.slice(1)), ([a, b]) => b > a));

const windowed = zipWith(sum, depths, depths.slice(1), depths.slice(2))
console.log(countMatches(zip(windowed, windowed.slice(1)), ([a, b]) => b > a));
