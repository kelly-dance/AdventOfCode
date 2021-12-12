import { DefaultMap, readAdvent, SortedSet } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(s => s.split('-'));

const cons = new DefaultMap<string, string[]>(() => []);
for(const [t,f] of inp){
  cons.get(t).push(f);
  cons.get(f).push(t);
}

const cache = new Map<string, number>()
const searchFrom = (current: string, visited: SortedSet<string>, part2: boolean): number => {
  if(current === 'end') return 1;
  const set = visited.reduce((a, c) => `${a},${c}`, '');
  const key = `${current}:${set}:${part2}`;
  if(cache.has(key)) return cache.get(key)!;
  let paths = 0;
  for(const path of cons.get(current)){
    let notDoubled = part2;
    if(visited.has(path)){
      if(part2 && path !== 'start') notDoubled = false;
      else continue;
    }
    const isLower = path === path.toLowerCase();
    if(isLower) visited.add(path);
    paths += searchFrom(path, visited, notDoubled);
    if(isLower && !(notDoubled !== part2)) visited.remove(path);
  }
  cache.set(key, paths);
  return paths;
};

let start = performance.now();
const p1set = new SortedSet<string>((a,b) => a === b ? 0 : a < b ? -1 : 1);
p1set.add('start');
console.log(searchFrom('start', p1set, false));
const p2set = new SortedSet<string>((a,b) => a === b ? 0 : a < b ? -1 : 1);
p2set.add('start');
console.log(searchFrom('start', p2set, true));
console.log(performance.now() - start, 'ms');

