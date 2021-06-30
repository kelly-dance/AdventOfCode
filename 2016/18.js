import { readFile, countMatches } from '../tools.ts';

let cur = readFile('./inputs/18.txt').split('').map(c => c === '^')

const iterate = line => line.map((_, i, o) => (o[i-1] ?? false) != (o[i+1] ?? false))

let total = 0;
for(let i = 0; i < 400000; i++){
  if(i === 40) console.log(total)
  total += countMatches(cur, b => !b)
  cur = iterate(cur)
}

console.log(total)
