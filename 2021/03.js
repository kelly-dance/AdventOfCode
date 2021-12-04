import { countMatches, range, readAdvent, sum, transpose } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(l => l.split('').map(s => parseInt(s)));

const counts = range(inp[0].length).map(()=>0);

const toDec = arr => arr.reduce((acc, cur) => acc * 2 + cur, 0);

const gamma = toDec(transpose(inp).map(bits => sum(bits) >= bits.length / 2));
const epsilon = ~gamma & (2 ** counts.length - 1);

console.log(gamma * epsilon);

let oxy = toDec(range(inp[0].length).reduce((acc, b) => {
  const count = countMatches(acc, e => e[b] === 1);
  return acc.filter(e => !!e[b] === (count >= acc.length / 2));
}, inp)[0]);

let co2 = toDec(range(inp[0].length).reduce((acc, b) => {
  if(acc.length === 1) return acc;
  const count = countMatches(acc, e => e[b] === 1);
  return acc.filter(e => !!e[b] !== (count >= acc.length / 2));
}, inp)[0]);

console.log(oxy * co2)
