import { readFile } from '../tools.ts';
import { prepareState, run } from './intcode.ts';

const program = readFile('./inputs/02.txt').split(',').map(s => parseInt(s));

const part1 = prepareState(program);

part1.memory.set(1n, 12n);
part1.memory.set(2n, 2n);

run(part1);

console.log(part1.memory.get(0n))

outer: for(let noun = 0; noun < 100; noun++){
  for(let verb = 0; verb < 100; verb++){
    const test = prepareState(program);
    test.memory.set(1n, BigInt(noun));
    test.memory.set(2n, BigInt(verb));
    run(test);
    if(test.memory.get(0n) === 19690720n){
      console.log(noun * 100 + verb)
      break outer;
    }
  }
}
