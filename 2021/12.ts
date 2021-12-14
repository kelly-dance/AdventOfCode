import { readAdvent } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(s => s.split('-'));

const labelToBin = new Map<string, number>();
for(const pair of inp){
  for(const label of pair){
    if(labelToBin.has(label)) continue;
    labelToBin.set(label, labelToBin.size);
  }
}

const isLowerMap = new Array(labelToBin.size);
for(const [key, value] of labelToBin) isLowerMap[value] = key === key.toLowerCase();

const cons: number[][] = new Array(labelToBin.size);
for(const pair of inp){
  const [t, f] = pair.map(label => labelToBin.get(label)!);
  if(cons[t] === undefined) cons[t] = [f];
  else cons[t].push(f);
  if(cons[f] === undefined) cons[f] = [t];
  else cons[f].push(t);
}

const start = labelToBin.get('start')!;
const end = labelToBin.get('end')!;
const cache: (number | undefined)[] = new Array(1 << (Math.ceil(Math.log2(labelToBin.size)) + labelToBin.size));
const searchFrom = (current: number, visited: number, part2: boolean): number => {
  if(current === end) return 1;
  const key = (current << labelToBin.size) | (visited << 1) | Number(part2);
  if(cache[key] !== undefined) return cache[key]!;
  let paths = 0;
  for(const neighbor of cons[current]){ // for each connected node
    let notDoubled = part2;
    if(visited & (1 << neighbor)){ // checks if neighbor is in visited
      if(part2 && neighbor !== start) notDoubled = false; // if double is not used, use it
      else continue; // otherwise we cannot go to this node
    }
    paths += searchFrom(
      neighbor,
      isLowerMap[neighbor]! ? visited | (1 << neighbor) : visited, // add next node to visited if its lower
      notDoubled,
    );
  }
  cache[key] = paths;
  return paths;
};

console.log(searchFrom(start, 1 << start, false)); // part 1
console.log(searchFrom(start, 1 << start, true)); // part 2


