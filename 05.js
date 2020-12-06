import { readFile } from './tools.ts';

const ids = readFile('inputs/05.txt')
  .split('\r\n')
  .map(
    l => parseInt(
      l.split('').map(
        c => c === 'B' || c === 'R'
      ).map(Number).map(String).join(''),
      2
    )
  )

const max = Math.max(...ids);
console.log(max)

const seen = new Set();
ids.forEach(x => seen.add(x))

for(let i = 0; i < max; i++){
  if(seen.has(i - 1) && seen.has(i + 1) && !seen.has(i)) console.log(i)
}
