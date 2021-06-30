import { readFile, sum, group, range } from '../tools.ts';

const inp = readFile('./inputs/03.txt').split('\n').map(l => l.trim().split(/\s+/g).map(s => parseInt(s)))

let legal = 0;

for(const tri of inp){
  const max = Math.max(...tri);
  const otr = sum(tri) - max;
  if(otr > max) legal++;
}

console.log(legal)

const inp2 = group(readFile('./inputs/03.txt').split('\n').map(l => l.trim().split(/\s+/g).map(s => parseInt(s))), 3).map(g => range(3).map(i => [g[0][i], g[1][i], g[2][i]])).flat(1);

let legal2 = 0;

for(const tri of inp2){
  const max = Math.max(...tri);
  const otr = sum(tri) - max;
  if(otr > max) legal2++;
}

console.log(legal2)
