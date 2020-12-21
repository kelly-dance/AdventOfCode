import { deepCopyArray, id, range, readFile } from './tools.ts';

const width = 10;

const toString = tile => tile.map(l => l.join('')).join('\n');

const flip = tile => deepCopyArray(tile).reverse();

/** @typedef {string[][]} Grid */

/**
 * @typedef {{
 *  id: number,
 *  syms: Grid[],
 *  edges: string[][],
 *  relations: {[x: number]: boolean[][]},
 *  right: undefined | null | Node,
 *  down: undefined | null | Node,
 *  up: undefined | null | Node,
 *  left: undefined | null | Node,
 *  chosen: Grid,
 * }} Node
 */

const rotate = (tile, w = width) => {
  const copy = deepCopyArray(tile);
  for(let x = 0; x < w; x++){
    for(let y = 0; y < w; y++){
      const nx = (- y + (w - 1) / 2) + (w - 1) / 2;
      const ny = x - (w - 1) / 2 + (w - 1) / 2;
      copy[nx][ny] = tile[x][y]
    }
  }
  return copy;
}

/** @type {{[x: number]: Node}} */
const input = Object.fromEntries(readFile('inputs/20.txt').split('\r\n\r\n').map(s => {
  const [title, ...data] = s.split('\r\n');
  let asArr = data.map(l => l.split(''));

  let flipped = flip(asArr);
  const set = new Set();
  set.add(toString(asArr))
  set.add(toString(flipped))
  for(let i = 0; i < 3; i++){
    asArr = rotate(asArr);
    flipped = rotate(flipped);
    set.add(toString(asArr))
    set.add(toString(flipped))
  }
  const syms = [...set].map(g => g.split('\n').map(l => l.split('')));
  const edges = syms.map(s => s[0]);
  const id = Number(title.substr(5,4));
  return [id, {
    id,
    syms,
    edges,
    relations: {},
    chosen: null,
  }]
}));

const megawidth = Math.sqrt(Object.keys(input).length)

for(const ai in input){
  const a = input[ai];
  for(const bi in input){
    const b = input[bi];
    if(a === b) continue;
    const matches = range(8).map(() => range(8).map(() => true));
    for(let i = 0; i < 8; i++){
      for(let j = 0; j < 8; j++){
        for(let k = 0; k < width; k++){
          if(a.edges[i][k] !== b.edges[j][k]){
            matches[i][j] = false;
            break;
          }
        }
      }
    }
    if(matches.some(m => m.some(id))){
      a.relations[bi] = matches;
    }
    
  }
}

/** @type {Node} */
let corner = null;

let acc = 1;
for(const key in input){
  const rels = Object.keys(input[key].relations).length;
  if(rels === 2) acc *= Number(key);
  if(rels === 2) corner = input[key];
}
console.log(acc)

let row = 1;
let colStart = corner;
while(true){
  let col = 1;
  let prev = colStart;
  while(true){
    
    /** @type {Node[]} */
    const known = ['left','up'].map(k => prev[k]).filter(Boolean);
    const neighbors = Object.entries(prev.relations);
    const comporator = prev.up?.right;
    const unused = neighbors.filter(([id]) => !known.some(otr => otr.id == id));
    let targetNeighbors = 4;
    if(row === 1 || row == megawidth) targetNeighbors--;
    if(col === megawidth - 1) targetNeighbors--;
    const choice = unused.find(([id]) => {
      /** @type {Node} */
      const choiceNode = input[id];
      const choiceNeighbors = Object.keys(choiceNode.relations).length;
      if(choiceNeighbors !== targetNeighbors) return false;
      if(comporator) return !!comporator.relations[id];
      return true;
    });
    /** @type {Node} */
    const choiceNode = input[choice[0]];
    
    prev.right = choiceNode;
    choiceNode.left = prev;
    choiceNode.up = comporator || null;
    if(comporator) comporator.down = choiceNode;
    prev = choiceNode;
    col++;
    if(col == megawidth) break;
  }
  const neighbors = Object.entries(colStart.relations);
  const known = ['right','up'].map(k => colStart[k]).filter(Boolean);
  const unused = neighbors.filter(([id]) => !known.some(otr => otr.id == id));
  if(!unused.length) break;
  const next = input[unused[0][0]];
  colStart.down = next;
  next.up = colStart;
  colStart = next;
  row++;
}

for(const value of Object.values(input)){
  
  const canidates = value.syms.filter(grid => {
    return ['up','down','left','right'].filter(s => value[s]).map(side => {
      /** @type {Grid[]} */
      const otrSyms = value[side].syms;
      return otrSyms.some(otr => {
        if(side === 'right'){
          for(let i = 0; i < width; i++){
            if(grid[i][width-1] !== otr[i][0]) return false;
          }
        }else if(side === 'left'){
          for(let i = 0; i < width; i++){
            if(grid[i][0] !== otr[i][width-1]) return false;
          }
        }else if(side === 'up'){
          for(let i = 0; i < width; i++){
            if(grid[0][i] !== otr[width-1][i]) return false;
          }
        }else if(side === 'down'){
          for(let i = 0; i < width; i++){
            if(grid[width-1][i] !== otr[0][i]) return false;
          }
        }
        return true;
      })
    }).every(id);
  })
  value.chosen = canidates[0];
}

let superwidth = (width - 2) * megawidth;
let finalMap = range(superwidth).map(x => range(superwidth).map(y => { 
  let downs = Math.floor(x / (width - 2));
  let rights = Math.floor(y / (width - 2));
  let cur = corner;
  for(let i = 0; i < downs; i++) cur = cur.down;
  for(let i = 0; i < rights; i++) cur = cur.right;
  let inX = (x % (width - 2)) + 1;
  let inY = (y % (width - 2)) + 1;
  return cur.chosen[inX][inY];
}));

const rots = [];
let copy = finalMap;
let flipped = flip(copy);
rots.push(copy)
rots.push(flipped)
for(let i = 0; i < 3; i++){
  copy = rotate(copy, superwidth);
  flipped = rotate(flipped, superwidth);
  rots.push(copy)
  rots.push(flipped)
}
const monster = `
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
`.split('\n').slice(1,4).map(l => l.split(''));

let tot = finalMap.flat(1).filter(c => c === '#').length;

for(const rot of rots){
  const hit = rot.map(r => r.map(c => c==='#'))
  let copy = deepCopyArray(rot);
  for(let x = 0; x < superwidth - monster.length; x++){
    for(let y = 0; y < superwidth - monster[0].length; y++){
      let every = true;
      outer:
      for(let i = 0; i < monster.length; i++){
        for(let j = 0; j < monster[0].length; j++){
          if(monster[i][j] === '#' && rot[x+i][y+j] !== '#') {
            every = false;
            break outer;
          }
        }
      }
      if(every){
        for(let i = 0; i < monster.length; i++){
          for(let j = 0; j < monster[0].length; j++){
            if(monster[i][j] === '#') {
              hit[x+i][y+j]=false;
              copy[x+i][y+j]='O'
            }
          }
        }
      }
    }
  }
  const score = hit.flat(1).filter(id).length;
  if(score !== tot) console.log(score)
}
