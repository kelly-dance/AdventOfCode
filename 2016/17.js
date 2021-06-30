import { Vec2, readFile, MD5 } from '../tools.ts';

const directions = {
  U: new Vec2(-1, 0),
  D: new Vec2(1, 0),
  L: new Vec2(0, -1),
  R: new Vec2(0, 1),
}

const getNeighbors = (vec, hash) => Object.entries(directions)
  .map(([dir, off]) => [dir, vec.add(off)])
  .filter((_, i) => 'bcdef'.includes(hash[i]))
  .filter(([_, pos]) => pos.x >= 0 && pos.x < 4 && pos.y >= 0 && pos.y < 4)

const password = readFile('./inputs/17.txt')

const queue = [['', new Vec2(0)]]
const lens = new Set();

while(queue.length){
  const [path, pos] = queue.shift();
  if(pos.x == 3 && pos.y == 3) {
    if(lens.size === 0) console.log(path)
    lens.add(path.length)
    continue;
  }
  const hash = MD5.strToStr(password + path).slice(0,4)
  const opts = getNeighbors(pos, hash);
  for(const [dir, pos] of opts){
    queue.push([path + dir, pos])
  }
}

console.log(Math.max(...lens))

