import { readFile, pickIntsFromString, range } from '../tools.ts';

/** @typedef {{
 *  id: string,
 *  size: number,
 *  used: number,
 *  goal?: boolean,
 *  fat?: boolean,
 * }} Unit */

/** @type {Unit[][]} */
const grid = range(37).map(() => range(27).map(() => ({size:null, used:null})))

let init0;

let tot = 0;
let totc = 0;
readFile('./inputs/22.txt').split('\n').slice(2).forEach(l => {
  const [x,y,size,used] = pickIntsFromString(l);
  if(used === 0) {
    init0 = [x, y];
  }
  grid[x][y].size = size;
  grid[x][y].used = used;
  grid[x][y].id = `${x},${y}`;
  tot += used;
  totc++;
});
const avg = tot / totc;
for(let x = 0; x < grid.length; x++){
  for(let y = 0; y < grid[x].length; y++){
    try{
      const o = grid[x][y];
      if(o.used / avg > 2) o.fat = true;
    }catch(e){

      console.log(x,y)
      throw e;
    }
    
  }
}

grid[grid.length - 1][0].goal = true;

let part1 = 0;

for(let x = 0; x < grid.length; x++){
  for(let y = 0; y < grid[x].length; y++){
    for(let x1 = 0; x1 < grid.length; x1++){
      for(let y1 = 0; y1 < grid[x].length; y1++){
        if((grid[x1][y1].size - grid[x1][y1].used) >= grid[x][y].used && grid[x][y].used > 0 && grid[x][y] !== grid[x1][y1]) part1++;
      }
    }
  }
}

console.log(part1)

/** @typedef {{grid:Unit[][], depth: number, goal: [number, number], zero: [number, number]}} State */

const checkBounds = (x,y) => {
  return x >= 0 && y >= 0 && x < grid.length && y < grid[x].length
}

/** @type {(state: State) => [[number, number], [number, number]][]} */
const getPairs = (state) => {
  const {grid, zero} = state;
  const [x0, y0] = zero;
  const results = [];
  const check = ([x,y], [x1,y1]) => {
    if(!checkBounds(x,y) || !checkBounds(x1, y1)) return;
    if(grid[x1][y1].size - grid[x1][y1].used >= grid[x][y].used) results.push([[x,y], [x1,y1]]);
  }
  check([x0 + 1, y0], zero);
  check([x0 - 1, y0], zero);
  check([x0, y0 + 1], zero);
  check([x0, y0 - 1], zero);
  return results;
}

/** @type {{state:Unit[][], depth: number, goal: [number, number]}[]} */
let queue = [{
  grid,
  depth: 0,
  goal: [grid.length - 1, 0],
  zero: init0,
}];

let seen = new Set();


let checks = 0;

/** @param {Unit[][]} grid */
const gridToS = (grid) => {
  return grid.map((r, x) => r.map((o, y) => {
    if(x === 0 && y === 0 && o.used === 0) return 'O';
    if(x === 0 && y === 0) return 'x';
    if(o.goal) return 'G'
    if(o.used === 0) return '_'
    if(o.fat) return '#'
    return '.'
  }).join(' ')).join('\n')
}

seen.add(gridToS(grid))

while(queue.length){
  checks++;
  /** @type {State} */
  const state = queue.shift();
  const {grid, depth, goal} = state;
  if(checks % 1000 === 0) console.log(checks, depth, queue.length)
  
  if(grid[0][0].goal === true){
    console.log(depth);
    break;
  }
  for(const [[x,y], [x1,y1]] of getPairs(state)){
    // const clone = grid.map(s => s.slice(0));
    const clone = grid.slice(0);
    let newGoal = goal;
    clone[x] = clone[x].slice(0);
    if(x !== x1) clone[x1] = clone[x1].slice(0);
    clone[x1][y1] = JSON.parse(JSON.stringify(clone[x1][y1]));
    clone[x][y] = JSON.parse(JSON.stringify(clone[x][y]));
    clone[x1][y1].used += clone[x][y].used;
    clone[x][y].used = 0;
    if(clone[x][y].goal || clone[x1][y1].goal){
      [clone[x][y].goal, clone[x1][y1].goal] = [clone[x1][y1].goal, clone[x][y].goal];
      newGoal = clone[x][y].goal ? [x, y] : [x1, y1];
    }
    // if(gridToS(grid).includes('O') || gridToS(clone).includes('O')){
    //   console.log({x,y,x1,y1, clone})
    //   console.log(gridToS(grid))
    //   console.log('vs')
    //   console.log(gridToS(clone))
    // }
    const asStr = gridToS(clone);
    if(seen.has(asStr)) continue;
    seen.add(asStr)
    
    queue.push({
      depth: depth + 1,
      grid: clone,
      goal: newGoal,
      zero: clone[x][y].used === 0 ? [x, y] : [x1, y1],
    })
  }
}

console.log(`ended after ${checks} checks`)
