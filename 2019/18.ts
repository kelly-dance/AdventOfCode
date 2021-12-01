import { readFile, Vec2, mapMap } from '../tools.ts';

const inp = readFile('./inputs/18.txt');

(() => { // Part 1
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

  type StepOption = { dist: number, reqs: string[], included: string[] };

  const distsFrom = (loc: Vec2): Map<string, StepOption> => {
    const dists = new Map<string, StepOption>();
    const seen: StepOption[][] = board.map(r => r.map(() => ({ dist: -1, reqs: [], included: [] })));
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
        newData.included = [...curData.included];
        if(invertedKeys.has(newp.toString())) {
          newData.included.push(invertedKeys.get(newp.toString())!);
          dists.set(invertedKeys.get(newp.toString())!, newData);
        }
        if(invertedDoors.has(newp.toString())) newData.reqs.push(invertedDoors.get(newp.toString())!);
        queue.push(newp);
      }
    }
    return dists;
  }

  const adjs = mapMap(keys, (_, value) => [value.toString(), distsFrom(value)]);
  adjs.set(start.toString(), distsFrom(start));

  type Result = [number, Vec2, Set<string>];
  const cache = new Map<`${number},${number},${string}`, Result>();

  const searchFrom = (pos: Vec2, curKeys: Set<string>): Result => {
    if(!adjs.has(pos.toString())) throw new Error('never');
    const sorted = [...curKeys].sort().join('');
    if(cache.has(`${pos.toString()},${sorted}`)) return cache.get(`${pos.toString()},${sorted}`)!;
    const curAdj = adjs.get(pos.toString())!;
    let best = [Infinity, Vec2.ORIGIN, new Set()] as Result;
    for(const [ key, { dist, reqs, included } ] of curAdj){
      if(curKeys.has(key) || !reqs.every(r => curKeys.has(r))) continue;
      const newKeys = included.filter(k => !curKeys.has(k));
      for(const k of newKeys) curKeys.add(k);
      const [subDist, subEndPos, subKeys] = searchFrom(keys.get(key)!, curKeys);
      const finDist = dist + subDist
      if(finDist < best[0])  best = [finDist, subEndPos, subKeys];
      for(const k of newKeys) curKeys.delete(k);
    }
    if(best[0] === Infinity) return [0, pos, new Set(curKeys)];
    cache.set(`${pos.toString()},${sorted}`, best);
    return best;
  }

  const [dist] = searchFrom(start, new Set());
  console.log(dist);
});

(() => { // Part 2
  let center: Vec2 = Vec2.ORIGIN;
  const keys = new Map<string, Vec2>();
  const doors = new Map<string, Vec2>();

  const board: boolean[][] = inp.split('\n').map((l, i) => {
    return l.split('').map((c, j) => {
      if(c === '@') center = new Vec2(i, j);
      if(/[a-z]/.test(c)) keys.set(c, new Vec2(i, j));
      if(/[A-Z]/.test(c)) doors.set(c.toLowerCase(), new Vec2(i, j));
      return c === '#';
    })
  });

  if(center === Vec2.ORIGIN) throw new Error('did not find center');
  
  [...Vec2.SIDES, Vec2.ORIGIN].map(v => v.add(center)).forEach(({x, y}) => board[x][y] = true);

  const starts = Vec2.DIAGONALS.map(v => v.add(center));

  const invertedKeys = mapMap(keys, (k, v) => [v.toString(), k]);
  const invertedDoors = mapMap(doors, (k, v) => [v.toString(), k]);

  type StepOption = { dist: number, reqs: string[], included: string[] };

  const distsFrom = (loc: Vec2): Map<string, StepOption> => {
    const dists = new Map<string, StepOption>();
    const seen: StepOption[][] = board.map(r => r.map(() => ({ dist: -1, reqs: [], included: [] })));
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
        newData.included = [...curData.included];
        if(invertedKeys.has(newp.toString())) {
          newData.included.push(invertedKeys.get(newp.toString())!);
          dists.set(invertedKeys.get(newp.toString())!, newData);
        }
        if(invertedDoors.has(newp.toString())) newData.reqs.push(invertedDoors.get(newp.toString())!);
        queue.push(newp);
      }
    }
    return dists;
  }

  const adjs = mapMap(keys, (_, value) => [value.toString(), distsFrom(value)]);
  starts.forEach(s => adjs.set(s.toString(), distsFrom(s)));
  

  type Result = [number, Vec2[], Set<string>];
  const cache = new Map<`${string},${string}`, Result>();

  const searchFrom = (poss: Vec2[], curKeys: Set<string>): Result => {
    const sorted = [...curKeys].sort().join('');
    console.log(poss.map(v => v.toString()).join(' '), [...curKeys].join(''))
    if(cache.has(`${poss.map(p => p.toString()).join(',')},${sorted}`)) return cache.get(`${poss.map(p => p.toString()).join(',')},${sorted}`)!;
    let best = [Infinity, new Array(4).fill(Vec2.ORIGIN), new Set()] as Result;
    for(let i = 0; i < poss.length; i++){
      const pos = poss[i];
      if(!adjs.has(pos.toString())) throw new Error('never');
      const curAdj = adjs.get(pos.toString())!;
      for(const [ key, { dist, reqs, included } ] of curAdj){
        if(curKeys.has(key) || !reqs.every(r => curKeys.has(r))) continue;
        const newKeys = included.filter(k => !curKeys.has(k));
        for(const k of newKeys) curKeys.add(k);
        const newPoss = poss.slice(0);
        newPoss[i] = keys.get(key)!;
        const [subDist, subEndPos, subKeys] = searchFrom(newPoss, curKeys);
        const finDist = dist + subDist
        if(finDist < best[0])  best = [finDist, subEndPos, subKeys];
        for(const k of newKeys) curKeys.delete(k);
      }
    }
    if(best[0] === Infinity) return [0, poss, new Set(curKeys)];
    cache.set(`${poss.map(p => p.toString()).join(',')},${sorted}`, best);
    return best;
  }

  const [dist] = searchFrom(starts, new Set());
  console.log(dist);
})();
