import { readFile, Vec2, invertMap, mapMap, mapFrom } from '../tools.ts';

const inp = readFile('./inputs/18.txt');

let start: Vec2 = Vec2.ORIGIN;
const keys = new Map<string, Vec2>();
const doors = new Map<string, Vec2>();

const board: boolean[][] = inp.split('\n').map((l, i) => {
  return l.split('').map((c, j) => {
    if(c === '@') start = new Vec2(i, j);
    if(/[a-z]/.test(c)) keys.set(c, new Vec2(i, j));
    if(/[A-Z]/.test(c)) doors.set(c.toLowerCase(), new Vec2(i, j));
    return c === '#';
  })
});

if(start === Vec2.ORIGIN) throw new Error('did not find start');

const invertedKeys = mapMap(keys, (k, v) => [v.toString(), k]);
const invertedDoors = mapMap(doors, (k, v) => [v.toString(), k]);

type StepOption = { dist: number, reqs: string[] };

const distsFrom = (loc: Vec2): Map<string, StepOption> => {
  const dists = new Map<string, StepOption>();
  const seen: StepOption[][] = board.map(r => r.map(() => ({ dist: -1, reqs: [] })));
  seen[loc.x][loc.y].dist = 0;
  const queue = [loc];
  while(queue.length){
    const cur = queue.shift()!;
    const curData = seen[cur.x][cur.y];
    for(const off of Vec2.SIDES){
      const newp = cur.add(off);
      if(board[newp.x][newp.y]) continue;
      const newData = seen[newp.x][newp.y];
      if(newData.dist !== -1) { 
        if(newData.dist <= curData.dist + 1) continue;
        if(newData.reqs.length <= curData.reqs.length) continue;
      }
      newData.dist = curData.dist + 1;
      newData.reqs = [...curData.reqs];
      if(invertedKeys.has(newp.toString())) dists.set(invertedKeys.get(newp.toString())!, newData);
      if(invertedDoors.has(newp.toString())) newData.reqs.push(invertedDoors.get(newp.toString())!);
      queue.push(newp);
    }
  }
  return dists;
}

const adjs = mapMap(keys, (_, value) => [value.toString(), distsFrom(value)]);
adjs.set(start.toString(), distsFrom(start));

let calls = 0;
const searchFrom = (pos: Vec2, curKeys: Set<string>, prev: number): number => {
  calls++;
  if(calls % 1000 === 0) console.log(calls)
  if(keys.size === curKeys.size) return prev;
  if(!adjs.has(pos.toString())) throw new Error('never');
  const curAdj = adjs.get(pos.toString())!;
  const opts = [...curAdj.entries()].filter(([k, v]) => !curKeys.has(k) && v.reqs.every(r => curKeys.has(r)));
  let best = Infinity;
  for(const [key, { dist} ] of opts){
    curKeys.add(key);
    const finDist = searchFrom(keys.get(key)!, curKeys, prev + dist);
    if(finDist < best) best = finDist;
    curKeys.delete(key);
  }
  return best;
}

console.log(searchFrom(start, new Set(), 0));
