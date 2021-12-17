import { pairs, pickIntsFromString, range, readAdvent } from '../tools.ts';

const [x1, x2, y1, y2] = pickIntsFromString(await readAdvent());

let validCount = 0;
let maxY = -Infinity;
for(let [vx, vy] of pairs(range(Math.floor((x1 * 2) ** 0.5), x2), range(y1, -y1))){
  let [x, y] = [0, 0]
  let localMaxY = 0;
  while(x <= x2 && y >= y1){
    if(y > localMaxY) localMaxY = y;
    if(x >= x1 && y <= y2){
      validCount++;
      if(localMaxY > maxY) maxY = localMaxY;
      break;
    }
    x += vx;
    y += vy;
    vx -= Math.sign(vx);
    vy -= 1;
  }
}
console.log(maxY, validCount);

