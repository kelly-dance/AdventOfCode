import { pickIntsFromString, readAdvent } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(pickIntsFromString);

const pts = new Set();
const overlaps = new Set();

const p1 = inp.filter(([a, b, x, y]) => a === x || b === y);
for(const [a, b, x, y] of p1){
  for(let i = Math.min(a,x); i <= Math.max(a,x); i++){
    for(let j = Math.min(b,y); j <= Math.max(b,y); j++){
      const s = `${i},${j}`;
      if(pts.has(s)) overlaps.add(s);
      pts.add(s);
    }
  }
}
console.log(overlaps.size);

const p2 = inp.filter(([a, b, x, y]) => !(a === x || b === y))
for(const [a, b, x, y] of p2){
  const dx = x > a ? 1 : -1;
  const dy = y > b ? 1 : -1;
  for(let i = 0; i <= Math.abs(a-x); i++){
    const s = `${a + dx * i},${ b + dy * i}`;
    if(pts.has(s)) overlaps.add(s);
    pts.add(s);
  }
}
console.log(overlaps.size);
