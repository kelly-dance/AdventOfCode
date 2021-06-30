import { readFile, sum, filterGenerator, combos, mapGenerator, takeFirst } from '../tools.ts';

const input = readFile('./inputs/02.txt').split('\n').map(l => l.split('\t').map(s => parseInt(s)))

console.log(sum(input.map(r => Math.max(...r) - Math.min(...r))))

console.log(sum(input.map(r => {
  for(const a of r){
    for(const b of r){
      if(a === b) continue;
      if(a % b === 0) return a / b;
    }
  }
  throw new Error('oof');
})));

console.log(
  sum(input.map(
    r => takeFirst(mapGenerator(
      filterGenerator(
        () => combos(r, 2),
        ([a, b]) => a !== b && a % b === 0
      ),
      ([a, b]) => a / b
    )())
  ))
)

