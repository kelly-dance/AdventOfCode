import { DefaultMap, readAdvent } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(s => s.split('-'));

const isLowerMap = new Map<number, boolean>();
const labelToBin = new Map<string, number>();
for(const pair of inp){
  for(const label of pair){
    if(labelToBin.has(label)) continue;
    const value = 1 << labelToBin.size;
    labelToBin.set(label, value);
    isLowerMap.set(value, label === label.toLowerCase());
  }
}

const cons = new DefaultMap<number, number[]>(() => []);
for(const [t,f] of inp){
  cons.get(labelToBin.get(t)!).push(labelToBin.get(f)!);
  cons.get(labelToBin.get(f)!).push(labelToBin.get(t)!);
}

let start = labelToBin.get('start')!;
let end = labelToBin.get('end')!;
const cache = new Map<number, number>()
const searchFrom = (current: number, visited: number, part2: boolean): number => {
  if(current === end) return 1;
  let key = (visited << 1) | (current << labelToBin.size) | Number(part2);
  if(cache.has(key)) return cache.get(key)!;
  let paths = 0;
  for(const path of cons.get(current)){
    let notDoubled = part2;
    if(visited & path){
      if(part2 && path !== start) notDoubled = false;
      else continue;
    }
    const changeSet = isLowerMap.get(path)! && !(notDoubled !== part2);
    if(changeSet) visited |= path;
    paths += searchFrom(path, visited, notDoubled);
    if(changeSet) visited &= ~path;
  }
  cache.set(key, paths);
  return paths;
};

let startTime = performance.now();
console.log(searchFrom(start, start, false)); // part 1
console.log(searchFrom(start, start, true)); // part 2
console.log(performance.now() - startTime, 'ms');

