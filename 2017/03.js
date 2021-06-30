import { readFile, Vec2, sum } from '../tools.ts';

const input = parseInt(readFile('./inputs/03.txt'));

const locate = n => {
  if(n === 1) return new Vec2(0, 0);
  const corners = [
    new Vec2(1, -1),
    new Vec2(1, 1),
    new Vec2(-1, 1),
    new Vec2(-1, -1),
  ]
  const directions = [
    new Vec2(0, 1),
    new Vec2(-1, 0),
    new Vec2(0, -1),
    new Vec2(1, 0),
  ]
  const r = Math.round((n - 1) ** 0.5 / 2);
  const start = (r * 2 - 1) ** 2
  const leftover = n - start - 1;
  const sideLen = r * 2
  const side = Math.floor(leftover / sideLen)
  const sideLeftover = leftover % sideLen + 1;
  return corners[side].mult(r).add(directions[side].mult(sideLeftover))
}

console.log(locate(input).manhattenLen())

const grid = new Map();
grid.set(`0,0`, 1);
let i = 2;
const dirs = [...Vec2.SIDES, ...Vec2.DIAGONALS];
while(true){
  const pos = locate(i);
  const score = sum(dirs.map(o => {
    const npos = pos.add(o);
    return grid.get(`${npos.x},${npos.y}`) || 0;
  }))
  if(score > input) {
    console.log(score);
    break;
  }
  grid.set(`${pos.x},${pos.y}`, score);
  i++;
}
