import { readFile, Vec2 } from '../tools.ts';

const dirs = {
  '^': new Vec2(1, 0),
  'v': new Vec2(-1, 0),
  '<': new Vec2(0, 1),
  '>': new Vec2(0, -1),
}

const inp = readFile('./inputs/03.txt').split('').map(c => dirs[c]);


// part 1
(()=>{
  let pos = new Vec2(0, 0);
  const seen = new Set();
  seen.add('0,0');
  for(const move of inp){
    pos = pos.add(move);
    seen.add(`${pos.x},${pos.y}`);
  }
  
  console.log(seen.size)
})();

// part 2
(()=>{
  let pos1 = new Vec2(0, 0);
  let pos2 = new Vec2(0, 0);
  const seen = new Set();
  seen.add('0,0');
  for(let i = 0; i < inp.length; i++){
    const move = inp[i];
    if(i % 2 === 0){
      pos1 = pos1.add(move);
      seen.add(`${pos1.x},${pos1.y}`);
    }else{
      pos2 = pos2.add(move);
      seen.add(`${pos2.x},${pos2.y}`);
    }
    
  }
  
  console.log(seen.size)
})();
