import { combos, countMatches, Grid, inRangeStd, readFile, sum } from './tools.ts';

/**
 * @typedef {(0 | 1 | 2)[][]} State
 * 0 = floor
 * 1 = empty
 * 2 = filled
 */

 /** @type {State} */
const input = readFile('inputs/11.txt').split('\r\n').map(l => l.split('').map(c => c === '.' ? 0 : 1));

// 8 directions to check in
const around = [
  [1,1],
  [1,0],
  [0,1],
  [-1,-1],
  [-1,0],
  [0,-1],
  [1,-1],
  [-1,1],
]

const neighborsPart1 = (state, x, y) => around.filter(([dx, dy]) => state[x + dx]?.[y + dy] === 2).length

const neighborsPart2 = (state, x, y) => around.filter(([dx, dy]) => {
  for(let d = 1; true; d++){
    const [nx, ny] = [x + dx * d, y + dy * d];
    if(!inRangeStd(nx, 0, state.length) || !inRangeStd(ny, 0, state[nx].length)) return false;
    if(state[nx][ny] === 1) return false;
    if(state[nx][ny] === 2) return true;
  }
}).length

/**
 * @param {State} frame 
 * @param {(state: State, x: number, y: number) => number} neighborFn 
 * @param {number} spawnReq 
 */
const update = (frame, neighborFn, spawnReq) => {
  const copy = frame.map(r => r.slice(0));
  let updated = false;
  for(let x = 0; x < frame.length; x++){
    for(let y = 0; y < frame[x].length; y++){
      const n = neighborFn(frame, x, y);
      if(frame[x][y] === 1 && n === 0) {
        copy[x][y] = 2;
        updated = true
      }
      if(frame[x][y] === 2 && n >= spawnReq) {
        updated = true
        copy[x][y] = 1;
      }
    }
  }
  return [updated, copy];
}

/**
 * @param {(state: State, x: number, y: number) => number} neighborFn 
 * @param {number} req 
 */
const solve = (neighborFn, req) => {
  let state = input.map(r => r.slice(0));
  // let ctr = 0;
  while(true){
    // console.log(ctr++);
    const [updated, newstate] = update(state, neighborFn, req);
    if(!updated) break;
    state = newstate;
  }
  return sum(state.map(r => r.filter(n => n === 2).length));
}

console.log(solve(neighborsPart1, 4));
console.log(solve(neighborsPart2, 5));
