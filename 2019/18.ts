import { readFile, Vec2, invertMap, mapMap } from '../tools.ts';

const inp = readFile('./inputs/18.txt');

let start: Vec2 | undefined = undefined;
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

if(!start) throw new Error('did not find start');

const invertedKeys = mapMap(keys, (k, v) => [v.toString(), k]);
const invertedDoors = mapMap(doors, (k, v) => [v.toString(), k]);

const distsFrom = (loc: Vec2): Record<string, { dist: number, reqs: string[] }> => {
  const dists = new Map<string, { dist: number, reqs: string[] }>();
  const seen: {dist: number, reqs: string[] }[][] = board.map(r => r.map(() => ({ dist: -1, reqs: [] })));
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
      if(invertedKeys.get(newp.toString())) dists.set(invertedKeys.get(newp.toString()), newData);
      if(invertedDoors.get(newp.toString())) newData.reqs.push(invertedDoors.get(newp.toString())!);
      queue.push(newp);
    }
  }
  return dists;
}

console.log(distsFrom(start))
