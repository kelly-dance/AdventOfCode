import { readFile, DefaultMap, Vec2 } from '../tools.ts';
import { scriptManager, prepareState } from './intcode.ts';

const program = readFile('./inputs/11.txt').split(',').map(s => parseInt(s));

(()=>{
  const paint = new DefaultMap<string, bigint>(() => 0n);
  let location = new Vec2(0);
  let direction = 0;
  
  scriptManager(prepareState(program), function*(provide, receive){
    while(true){
      const locString = `${location.x},${location.y}`;
      yield provide(paint.get(locString));
      const newColor = yield receive();
      paint.set(locString, newColor);
      const rotate = yield receive();
      if(rotate === 1n) direction = (direction + 1) % 4;
      else direction = (direction + 3) % 4;
      location = location.add(Vec2.SIDES[direction]);
    }
  });
  
  console.log([...paint.keys()].length - 1);
})();

(()=>{
  const paint = new DefaultMap<string, bigint>(() => 0n);
  paint.set('0,0', 1n);
  let location = new Vec2(0);
  let direction = 0;
  
  scriptManager(prepareState(program), function*(provide, receive){
    while(true){
      const locString = `${location.x},${location.y}`;
      yield provide(paint.get(locString));
      const newColor = yield receive();
      paint.set(locString, newColor);
      const rotate = yield receive();
      if(rotate === 1n) direction = (direction + 1) % 4;
      else direction = (direction + 3) % 4;
      location = location.add(Vec2.SIDES[direction]);
    }
  });

  for(let y = 0; y >= -5; y--){
    let s = '';
    for(let x = 1; x <= 39; x++){
      s += paint.get(`${x},${y}`) ? '#' : ' '
    }
    console.log(s);
  }
  
})();
