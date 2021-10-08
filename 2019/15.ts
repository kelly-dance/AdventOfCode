import { readFile, MultiMap, Vec2 } from '../tools.ts';
import * as int from './intcode.ts';

const program = readFile('./inputs/15.txt').split(',').map(s => parseInt(s));

type Dirs = 1n | 2n | 3n | 4n;
type QueueItem = {
  save: int.MachineState,
  inst: Dirs,
  pos: Vec2,
  dist: number,
}

const queue: QueueItem[] = [];

const choiceMap = new Map<Dirs, Vec2>();
choiceMap.set(1n, new Vec2(0, 1));
choiceMap.set(2n, new Vec2(0, -1));
choiceMap.set(3n, new Vec2(-1, 0));
choiceMap.set(4n, new Vec2(1, 0));

const map = new MultiMap<[number, number], bigint>(2);
for(const [inst, disp] of choiceMap) {
  queue.push({
    save: int.prepareState(program),
    inst,
    pos: disp,
    dist: 1,
  });
}

while(queue.length > 0){
  let cur = queue.shift()!;
  let res = int.scriptManager<bigint>(cur?.save, function*(prov, rec){
    yield prov(cur.inst);
    return yield rec();
  }, true);
  map.set([cur.pos.x, cur.pos.y], res);
  if(res === 2n) console.log("Part 1: " + cur.dist);
  if(res !== 0n){
    for(const [inst, disp] of choiceMap) {
      const newPos = cur.pos.add(disp);
      if(map.has([newPos.x, newPos.y])) continue;
      queue.push({
        save: int.cloneState(cur.save),
        dist: cur.dist+1,
        inst,
        pos: newPos,
      });
    }
  }
}
let minX = 0;
let maxX = 0;
let minY = 0;
let maxY = 0;
for(const [x, y] of map.keys()){
  if(x < minX) minX = x;
  if(x > maxX) maxX = x;
  if(y < minY) minY = y;
  if(y > maxY) maxY = y;
}

let out = new Array(maxX-minX+1).fill(0).map(e=>new Array(maxY-minY+1).fill(3));
for(const [x, y] of map.keys()){
  out[Number(x)-minX][Number(y)-minY] = Number(map.get([x, y]));
}

let oX = -1;
let oY = -1;
for(let x in out){
  for(let y in out[x]){
    if(out[x][y]==2){
      oX = Number(x);
      oY = Number(y);
    }
  }
}

let bfs = [{x:oX,y:oY}];
let seen2 = new Set();
let depth = 0;
while(bfs.length!=0){
  depth++;
  let next = [];
  while(bfs.length !== 0){
    let cur = bfs.pop()!;
    if(out[cur.x][cur.y]==0) continue;
    else out[cur.x][cur.y] = 2;
    for(let dir of choiceMap.values()){
      let x = cur.x + dir.x;
      let y = cur.y + dir.y;
      if(seen2.has(`${x},${y}`)) continue;
      else seen2.add(`${x},${y}`);
      next.push({ x, y });
    }
  }
  bfs = next;
}
console.log("Part 2: "+(depth-1));
