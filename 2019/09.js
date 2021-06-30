import { readFile } from '../tools.ts';
import { scriptManager, prepareState } from './intcode.ts';

const program = readFile('./inputs/09.txt').split(',').map(s => parseInt(s));

scriptManager(prepareState(program), function*(provide, receive){
  yield provide(1n);
  const output = yield receive();
  console.log(output.toString());
});

scriptManager(prepareState(program), function*(provide, receive){
  yield provide(2n);
  const output = yield receive();
  console.log(output.toString());
});
