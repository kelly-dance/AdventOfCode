import { pickIntsFromString, readAdvent } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(pickIntsFromString);

const covered = new Set();
const overlaps = new Set();

const processPoints = pts => {
  for(const [a, b, x, y] of pts){
    for(let i = 0; i <= Math.max(Math.abs(a-x), Math.abs(b-y)); i++){
      const s = `${a + i * Math.sign(x - a)},${b + i * Math.sign(y - b)}`;
      if(covered.has(s)) overlaps.add(s);
      covered.add(s);
    }
  }
}

processPoints(inp.filter(([a, b, x, y]) => a === x || b === y));
console.log(overlaps.size);

processPoints(inp.filter(([a, b, x, y]) => a !== x && b !== y));
console.log(overlaps.size);
