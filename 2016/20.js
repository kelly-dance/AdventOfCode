import { readFile, sum } from '../tools.ts';

const inp = readFile('./inputs/20.txt').split('\n').map(line => line.split('-').map(n => parseInt(n)));

let segs = [[0, 4294967295]];

for(const [start, end] of inp){
  const startSegI = segs.findIndex(([segStart, segEnd]) => segEnd >= start);
  const endSegI = segs.length - 1 - segs.slice(0).reverse().findIndex(([segStart, segEnd]) => segStart <= end);
  if(startSegI > endSegI || startSegI === -1 || endSegI === segs.length) continue;
  if(startSegI === endSegI) {
    const temp = segs[startSegI][1];
    segs[startSegI][1] = start - 1;
    segs.push([end + 1, temp]);
  }else{
    segs[startSegI][1] = start - 1;
    segs[endSegI][0] = end + 1;
    segs = segs.filter((_, i) => i <= startSegI || i >= endSegI)
  }
  segs = segs.filter(([s, e]) => e >= s).sort((a,b) => a[0] < b[0] ? -1 : 1)
}

console.log(segs[0][0])
console.log(sum(segs.map(([start, end]) => end - start + 1)))
