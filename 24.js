import { countMatches, mod, MultiMap, readFile } from './tools.ts';

/** @type {MultiMap<[number, number], boolean>} */
const mm = new MultiMap(2);

readFile('./inputs/24.txt').split('\r\n').forEach(l => {
  let pos = [0, 0];
  let i = 0;
  while(i < l.length){
    switch(l.charAt(i)){
      case 'n':
      case 's':
        const s = l.charAt(i) === 'n' ? 1 : -1;
        switch(l.charAt(i + 1)){
          case 'e':
            pos = [pos[0] + s, pos[1] + (pos[0] % 2 ? 1 : 0)]
            break;
          case 'w':
            pos = [pos[0] + s, pos[1] - (pos[0] % 2 ? 0 : 1)]
            break;
        }
        i += 2;
        break;
      case 'e':
        i++;
        pos = [pos[0], pos[1] + 1]
        break;
      case 'w':
        i++;
        pos = [pos[0], pos[1] - 1]
        break;
    }
  }
  mm.set(pos, !(mm.get(pos) || false))
});

const count = map => {
  let tot = 0;
  for(const value of map.values()){
    if(value) tot++;
  }
  return tot;
}

console.log(count(mm));

const print = (map, size = 5) => {
  for(let i = -size; i <= size; i++){
    let r = mod(i, 2) ? ' ' : '';
    for(let j = -size; j <= size; j++){
      r += (map.get([i,j]) ? '#' : '.') + ' '
    }
    console.log(r)
  }
}

const neighbors = key => [
  [key[0], key[1]+1],
  [key[0], key[1]-1],
  [key[0]+1, key[1]],
  [key[0]-1, key[1]],
  ...(
    (key[0] % 2 === 0) ? [
      [key[0]+1, key[1]-1],
      [key[0]-1, key[1]-1],
    ] : [
      [key[0]+1, key[1]+1],
      [key[0]-1, key[1]+1],
    ]
  ),
];

let prev = mm;
for(let day = 0; day < 100; day++){
  /** @type {MultiMap<[number, number], boolean>} */
  const next = new MultiMap(2);
  const checked = new Set();
  const process = pair => {
    const asStr = pair.join();
    if(checked.has(asStr)) return;
    checked.add(asStr);
    const numNeighbors = countMatches(neighbors(pair), k => prev.get(k));
    if(prev.get(pair)){
      if(numNeighbors === 0 || numNeighbors > 2) next.set(pair, false);
      else next.set(pair, true);
    }else{
      if(numNeighbors === 2) next.set(pair, true);
    }
  }
  for(const [key, value] of prev.entries()){
    if(!value) continue;
    process(key);
    neighbors(key).forEach(process);
  }
  prev = next;
}
// print(prev, 80)
console.log(count(prev))
