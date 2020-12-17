import { readFile, combos, zipWith, sum, countMatches } from './tools.ts';

const input = readFile('inputs/17.txt').split('\r\n').map(l => l.split('').map(c => [c === '#']));

const addToSet = (set, dimensions, coords, ...prev) => {
  if(!Array.isArray(coords)) {
    if(coords) set.add(prev.join(',')+',0'.repeat(dimensions - prev.length));
  } else {
    const sub = Math.floor(coords.length / 2);
    coords.forEach((a, i) => addToSet(set, dimensions, a, ...prev, i - sub))
  }
}

const solve = (dimensions, input) => {
  let prev = new Set();
  addToSet(prev, dimensions, input);
  for(let i = 0; i < 6; i++){
    const checked = new Set();
    const next = new Set();
    for(const str of prev.keys()){
      const pos = str.split(',').map(Number)
      for(const offset of combos([-1,0,1], dimensions)) {
        const newPos = zipWith(sum, pos, offset);
        if(newPos.some((n, i) => i > 1 && n < 0)) continue;
        const asStr = newPos.join(',');
        if(checked.has(asStr)) continue;
        checked.add(asStr);
        let neighbors = 0;
        for(const offset2 of combos([-1,0,1], dimensions)) {
          if(offset2.every(n => n === 0)) continue;
          const neighborPos = zipWith(sum, newPos, offset2).map((n, i) => i > 1 ? Math.abs(n) : n);
          const neighborAsStr = neighborPos.join(',');
          if(prev.has(neighborAsStr)) neighbors++;
        }
        if(prev.has(asStr)){
          if(neighbors === 2 || neighbors === 3) next.add(asStr);
        }else{
          if(neighbors === 3) next.add(asStr);
        }
      }
    }
    prev = next
  }
  
  let c = 0;
  for(const value of prev.values()){
    let mirrors = countMatches(value.split(',').map(Number), (c, i) => i > 1 && c > 0);
    c += 2 ** mirrors;
  }
  console.log(c)
}

solve(3, input)
solve(4, input)
