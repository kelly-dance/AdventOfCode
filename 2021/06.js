import { memoize, readAdvent, sum } from '../tools.ts';

const inp = (await readAdvent()).split(',').map(s => parseInt(s));

const calc = memoize(remdays => remdays <= 0 ? 1 : calc(remdays - 7) + calc(remdays - 9));

console.log(sum(inp.map(f => calc(80 - f))));
console.log(sum(inp.map(f => calc(256 - f))));
