import { readFile, range, pairs, zip } from '../tools.ts';

const input = parseInt(readFile('./inputs/11.txt'));

const fuel = range(1,301).map(x => range(1, 301).map(y => {
  const rackID = x + 10;
  let powerLvl = rackID * y;
  powerLvl += input;
  powerLvl *= rackID;
  powerLvl = Math.floor(powerLvl / 100) % 10;
  powerLvl -= 5;
  return powerLvl;
}))

let bestScore = -Infinity
let bestLoc = undefined
let bestSize = undefined
for(const size of range(1, 301)){
  console.log(size)
  for(const [x, y] of pairs(range(1, 301 - size), range(1, 301 - size))){
    const score = pairs(range(size), range(size)).reduce((acc, [dx,dy]) => {
      return acc + fuel[x + dx - 1][y + dy - 1]
    }, 0)
    if(score > bestScore) {
      bestScore = score
      bestLoc = [x,y];
      bestSize = size
    }
  }
}
console.log(bestScore, bestLoc, bestSize)
