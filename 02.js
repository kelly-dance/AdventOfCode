import { inRange, readFile } from './tools.ts';

const input = readFile('./inputs/02.txt').split('\n').map(l => {
  const matches = l.match(/(\d+)-(\d+) ([a-z]): ([a-z]+)/).slice(1);
  return {
    min: Number(matches[0]),
    max: Number(matches[1]),
    ltr: matches[2],
    str: matches[3],
  }
})

console.log(input.filter(l => inRange(l.str.split('').filter(c => c === l.ltr).length, l.min, l.max, true)).length)
console.log(input.filter(l => (l.str.charAt(l.min - 1) === l.ltr) ^ (l.str.charAt(l.max - 1) === l.ltr)).length)
