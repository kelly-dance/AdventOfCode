import { readFile, Vec2 } from '../tools.ts';

type Segment = {
  start: Vec2,
  end: Vec2,
  prevLen: number,
}

const dirs = {
  U: new Vec2(1, 0),
  D: new Vec2(-1, 0),
  L: new Vec2(0, -1),
  R: new Vec2(0, 1),
}

const segments: Segment[][] = [];

readFile('./inputs/03.txt').split('\n').forEach(l => {
  const steps = l.split(',').map(s => dirs[s[0] as keyof typeof dirs].mult(parseInt(s.substring(1))));
  const curSegments: Segment[] = [];
  let loc = new Vec2(0);
  let prevLen = 0;
  for(const step of steps){
    const newLoc = loc.add(step)
    curSegments.push({
      start: loc,
      end: newLoc,
      prevLen,
    })
    prevLen += step.len();
    loc = newLoc
  }
  segments.push(curSegments);
});

const intersects = (a: Segment, b: Segment): undefined | Vec2 => {
  if((a.start.x === a.end.x && b.start.x === b.end.x) || (a.start.y === a.end.y && b.start.y === b.end.y)) return;
  if(
    a.start.x !== a.end.x &&
    b.start.x === b.end.x &&
    Math.min(a.start.x, a.end.x) < b.start.x &&
    b.start.x < Math.max(a.start.x, a.end.x) &&
    Math.min(b.start.y, b.end.y) < a.start.y &&
    a.start.y < Math.max(b.start.y, b.end.y)
  ) return new Vec2(b.start.x, a.start.y);
  if(
    a.start.x === a.end.x &&
    b.start.x !== b.end.x &&
    Math.min(a.start.y, a.end.y) < b.start.y &&
    b.start.y < Math.max(a.start.y, a.end.y) &&
    Math.min(b.start.x, b.end.x) < a.start.x &&
    a.start.x < Math.max(b.start.x, b.end.x)
  ) return new Vec2(a.start.x, b.start.y);
}

const [aSegs, bSegs] = segments;

let part1 = Infinity;
let part2 = Infinity;
for(const a of aSegs){
  for(const b of bSegs){
    const point = intersects(a, b);
    if(point){
      if(point.manhattenLen() < part1) part1 = point.manhattenLen();
      const aLen = point.sub(a.start).len() + a.prevLen
      const bLen = point.sub(b.start).len() + b.prevLen
      const totLen = aLen + bLen;
      if(totLen < part2) part2 = totLen;
    }
  }
}
console.log(part1)
console.log(part2)
