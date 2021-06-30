import { Vec2, readFile } from '../tools.ts';

const inp = readFile('./inputs/02.txt').split('\n').map(s => s.split(''));

const dirs = {
  'U': new Vec2(-1, 0),
  'D': new Vec2(1, 0),
  'L': new Vec2(0, -1),
  'R': new Vec2(0, 1),
};

// Part 1

(()=>{
  const keypad = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
  ]
  
  let pos = new Vec2(1, 1);
  
  let fin = '';
  
  for(const line of inp){
    for(const move of line){
      const newPos = pos.add(dirs[move]);
      if(newPos.x < 0 || newPos.x >= 3 || newPos.y < 0 || newPos.y >= 3) continue;
      pos = newPos;
    }
    fin += keypad[pos.x][pos.y];
  }
  console.log(fin)
})();

// Part 2

(()=>{
  const keypad = [
    [undefined, undefined, '1', undefined, undefined],
    [undefined, '2', '3', '4', undefined],
    ['5', '6', '7', '8', '9'],
    [undefined, 'A', 'B', 'C', undefined],
    [undefined, undefined, 'D', undefined, undefined],
  ]
  
  let pos = new Vec2(2, 0);
  
  let fin = '';
  
  for(const line of inp){
    for(const move of line){
      const newPos = pos.add(dirs[move]);
      if(!keypad[newPos.x]?.[newPos.y]) continue;
      pos = newPos;
    }
    fin += keypad[pos.x][pos.y];
  }
  console.log(fin)
})();
