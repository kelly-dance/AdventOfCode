import { readFile, nonRepeatingCombosWithLowerTimes, countMatches } from '../tools.ts';

/** @typedef {{material: string, type: 'generator' | 'microchip'}} Obj */
/** @typedef {Obj[][]} BuildingState */

/** @type {BuildingState} */
const input = readFile('./inputs/11.txt').split('\n').map(s => {
  const rest = s.substring('contains '.length + s.indexOf('contains'), s.length - 1);
  if(rest === 'nothing relevant') return [];
  else {
    const entries = rest.split(/(,? and )|(, )/)
      .filter(s => s && s !== ', ' && s !== ' and ' && s !== ', and ')
      .map(s => s.substring(2))
      .map(s => s.split(' '))
      .map(([material, type]) => ({ material: material.split('-')[0], type }))
      .sort((a,b) => {
        if(a.material < b.material) return -1;
        if(a.material > b.material) return 1;
        return a.type < b.type ? -1 : 1;
      });
    return entries
  }
});

/** @typedef {{ floor: number, building: BuildingState}} State */

/** @type {State} */
const init = {
  floor: 0,
  building: input,
}

let queue = [init];

let d = -1;
let checks = 0;
let bestTop = 0;

const seenStates = new Set();
seenStates.add(JSON.stringify(init));
console.log(init)
init.depth = 0;

outer:
while(queue.length){
  /** @type {State} */
  const state = queue.shift();
  const { floor, building, depth } = state;
  if(depth > d){
    d = depth;
    console.log(d);
  }
  
  checks++;
  if(checks % 10000 === 0) console.log(checks, seenStates.size);
  if(floor === 3 && building.slice(0, 3).every(f => f.length === 0)) {
    console.log({
      checks,
      floor,
      building,
      depth,
    })
    break;
  }
  if(floor === 3 && building[floor].length > bestTop){
    bestTop = building[floor].length;
    queue = queue.filter(s => s.building[floor].length > bestTop - 2);
  }
  /** @type {Record<string, Record<Obj['type'], boolean>} */
  const mats = {};
  let floorHasAnyGen = false;
  for(const object of building[floor]){
    if(object.type === 'generator') floorHasAnyGen = true;
    if(!(object.material in mats)) mats[object.material] = { generator: false, chip: false };
    mats[object.material][object.type] = true;
  }
  if(floorHasAnyGen){
    for(const pair of Object.values(mats)){
      if(pair.microchip && !pair.generator) {
        continue outer;
      }
    }
  }
  for(const moving of nonRepeatingCombosWithLowerTimes(building[floor], 2)){
    for(const dir of [-1, 1]){
      if(dir === -1 && moving.length === 2) continue;
      const newFloor = floor + dir;
      if(newFloor < 0 || newFloor > 3) continue;
      const clone = building.slice(0);
      clone[floor] = clone[floor].filter(o => !moving.includes(o));
      clone[newFloor] = [...clone[newFloor], ...moving]
        .sort((a,b) => {
          if(a.material < b.material) return -1;
          if(a.material > b.material) return 1;
          return a.type < b.type ? -1 : 1;
        });
      const newState = {
        floor: newFloor,
        building: clone,
      };
      const asString = JSON.stringify(newState);
      if(seenStates.has(asString)) continue;
      else seenStates.add(asString);
      newState.depth = depth + 1;
      queue.push(newState);
    }
  }
}

