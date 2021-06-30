import { readFile } from '../tools.ts';
import { prepareState, scriptManager } from './intcode.ts';

const program = readFile('./inputs/05.txt').split(',').map(s => parseInt(s));

scriptManager(prepareState(program), function*(provide, receive){
  yield provide(1n);
  for(let i = 0; i < 9; i++) yield receive();
  const result = yield receive()
  console.log(result.toString())
});

scriptManager(prepareState(program), function*(provide, receive){
  yield provide(5n);
  const result = yield receive()
  console.log(result.toString())
});
