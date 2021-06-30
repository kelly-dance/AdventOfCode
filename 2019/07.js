import { readFile, permutations, range } from '../tools.ts';
import { scriptManager, prepareState, multiScriptManager } from './intcode.ts';

const program = readFile('./inputs/07.txt').split(',').map(s => parseInt(s));

let part1 = 0n;

for(const order of permutations(range(5))){
  let state = 0n;
  for(const amp of order){
    scriptManager(prepareState(program), function*(provide, receive){
      yield provide(BigInt(amp));
      yield provide(state);
      state = yield receive();
    })
  }
  if(state > part1) part1 = state;
}

console.log(part1);

let part2 = 0n;

for(const order of permutations(range(5, 10))){
  let state = 0n;
  multiScriptManager(order.map(() => prepareState(program)), function*(computers){
    for(let i = 0; i < 5; i++){
      yield computers[i].provide(BigInt(order[i]))
    }
    while(true){
      for(let i = 0; i < 5; i++){
        yield computers[i].provide(state);
        state = yield computers[i].receive();
      }
    }
  });
  if(state > part2) part2 = state;
}

console.log(part2);
