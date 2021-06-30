import { Vec2, readFile } from '../tools.ts';

const inp = readFile('./inputs/01.txt').split(', ').map(s => ({ dir: s[0], dist: parseInt(s.substring(1))}))

let pos = new Vec2(0, 0)
let dir = 0;

const dirs = [
  new Vec2(0, 1),
  new Vec2(1, 0),
  new Vec2(0, -1),
  new Vec2(-1, 0),
]

let p2 = false;
const seen = new Set();

for(const step of inp){
  if(step.dir === 'R') dir = (dir + 1) % 4;
  else dir = (dir + 3) % 4;
  for(let i = 0; i < step.dist; i++){
    if(seen.has(`${pos.x},${pos.y}`) && !p2) {
      console.log('Part 2', pos, Math.abs(pos.x) + Math.abs(pos.y))
      p2 = true;
    }
    seen.add(`${pos.x},${pos.y}`)
    pos = pos.add(dirs[dir])
  }
  
}

console.log('Part 1', pos, Math.abs(pos.x) + Math.abs(pos.y));
