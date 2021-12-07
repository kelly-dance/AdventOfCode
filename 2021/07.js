import { memoize, readAdvent, range, sum } from '../tools.ts';

const inp = (await readAdvent()).split(',').map(s => parseInt(s));

const trinums = memoize(v => v <= 1 ? v : v + trinums(v - 1));

console.log(Math.min(...range(Math.max(...inp)).map(p => sum(inp.map(v => Math.abs(v - p))))));
console.log(Math.min(...range(Math.max(...inp)).map(p => sum(inp.map(v => trinums(Math.abs(v - p)))))));

