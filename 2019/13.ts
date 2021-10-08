import * as int from './intcode.ts';
import { readFile, MultiMap } from '../tools.ts';

const prog = readFile('./inputs/13.txt').split(',').map(s => parseInt(s));

// part 1
const board1 = new MultiMap<[bigint, bigint], bigint>(2);

int.scriptManager(int.prepareState(prog), function*(provide, receive){
  while(true){
    const x = yield receive();
    const y = yield receive();
    const id = yield receive();
    board1.set([x, y], id);
  }
});

let count = 0;
for(const key of board1.values()){
  if(key === 2n) count++;
}
console.log(count);

// part 2
prog[0] = 2;

const board2 = new MultiMap<[bigint, bigint], bigint>(2);

let score = 0n;

int.scriptManager(int.prepareState(prog), function*(provide, receive, type){
  while(true){
    const mode = yield type();
    if(mode === int.ScriptIOMode.receive){
      const x = yield receive();
      const y = yield receive();
      const id = yield receive();
      if(x === -1n && y === 0n) score = id;
      else board2.set([x, y], id);
    }else{
      let ball: bigint | undefined = undefined;
      let paddle: bigint | undefined = undefined;
      let x = 0n;
      while(ball === undefined || paddle === undefined){
        for(let y = 0n; y < 50n; y++){
          if(board2.get([x,y]) === 3n) paddle = x;
          if(board2.get([x,y]) === 4n) ball = x;
        }
        x++;
      }
      yield provide(ball > paddle ? 1n : (paddle === ball ? 0n : -1n));
    }
  }
});

console.log(score);
