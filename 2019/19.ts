import { readAdvent, memoizeMulti } from '../tools.ts';
import * as int from './intcode.ts';

const prog = (await readAdvent()).split(',').map(s => parseInt(s));

const isOn = memoizeMulti((x: number, y: number): boolean => {
  if(x < 0 || y < 0) return false;
  let ret = 0n;
  int.scriptManager(int.prepareState(prog), function*(send, rec){
    yield send(BigInt(x));
    yield send(BigInt(y));
    ret = yield rec();
  })
  return !!ret;
})

let count = 0;
for(let x = 0; x < 50; x++){
  for(let y = 0; y < 50; y++){
    if(isOn(x, y)) count++;
  }
}
console.log(count);

const lostFrom = (x: number, y: number): [number, number] => {
  let dist = 1;
  while(true){
    for(let a = 0; a < dist; a++){
      if(isOn(x + dist, y + a)) return [x + dist, y + a];
      if(isOn(x + a, y + dist)) return [x + a, y + dist];
    }
    if(isOn(x + dist, y + dist)) return [x + dist, y + dist];
    dist++;
  }
}

let x = 0;
let y = 0;
while(true){
  if(isOn(x, y)) {
    if(isOn(x - 99, y + 99)) {
      console.log(10000 * (x - 99) + y);
      break;
    }
    if(isOn(x+1,y)) x++;
    else if(isOn(x, y+1)) y++;
    else [x, y] = lostFrom(x, y);
  } else [x, y] = lostFrom(x, y);
}
