import { readAdvent, Vec2, Vec2Str, mapMap } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(l => l.split(''));

const singly = new Map<string, Vec2>();
const portals = new Map<Vec2Str, Vec2>();
const maze: boolean[][] = inp.map((l, x) => l.map((c, y) => {
  if(/[A-Z]/.test(c)){
    const dirs = [
      [inp[x][y+1], inp[x][y+2], new Vec2(0, 2)],
      [inp[x][y+1], inp[x][y-1], new Vec2(0, -1)],
      [inp[x+1]?.[y], inp[x+2]?.[y], new Vec2(2, 0)],
      [inp[x+1]?.[y], inp[x-1]?.[y], new Vec2(-1, 0)],
    ] as [string, string, Vec2][];
    const dir = dirs.find(([f, s]) => f && /[A-Z]/.test(f) && s === '.');
    if(dir){
      const label = c + dir[0];
      const loc = new Vec2(x, y).add(dir[2]);
      if(singly.has(label)){
        portals.set(loc.toString(), singly.get(label)!);
        portals.set(singly.get(label)!.toString(), loc);
      }else{
        singly.set(label, loc);
      }
    }
  }
  return c !== '.';
}));

(()=>{ // Part 1
  const seen = new Set<Vec2Str>();
  const queue = [[singly.get('AA')!, 1]] as [Vec2, number][];
  outer: while(queue.length){
    const [cur, dist] = queue.shift()!;
    const opts = Vec2.SIDES.map(o => cur.add(o)).filter(v => !maze[v.x][v.y]);
    if(portals.has(cur.toString())) opts.push(portals.get(cur.toString())!);
    for(const opt of opts){
      if(seen.has(opt.toString())) continue;
      seen.add(opt.toString())
      if(singly.get('ZZ')?.eq(opt)) {
        console.log(dist)
        break outer;
      }
      queue.push([opt, dist + 1]);
    }
  }
})();

(()=>{ // Part 2
  const start = singly.get('AA')!;
  const seen = new Set<`${Vec2Str},${number}`>();
  const queue = [[start, 1, 0]] as [Vec2, number, number][];
  outer: while(queue.length){
    const [cur, dist, lvl] = queue.shift()!;
    const opts = Vec2.SIDES.map(o => cur.add(o)).filter(v => !maze[v.x][v.y]).map(o => [o, lvl]) as [Vec2, number][];
    if(portals.has(cur.toString())) {
      const to = portals.get(cur.toString())!;
      const isOuter = cur.x < 5 || cur.y < 5 || cur.x > maze.length - 5 || cur.y > maze[0].length - 5;
      if(lvl !== 0 || !isOuter) opts.push([to, lvl + (isOuter ? 1 : -1)]);
    }
    for(const [loc, dlvl] of opts){
      if(seen.has(`${loc.toString()},${dlvl}`)) continue;
      seen.add(`${loc.toString()},${dlvl}`)
      if(singly.get('ZZ')?.eq(loc) && dlvl === 0) {
        console.log(dist)
        break outer;
      }
      queue.push([loc, dist + 1, dlvl]);
    }
  }
})();

