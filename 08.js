import { readFile } from './tools.ts';

/** @typedef {["acc"|"nop"|"jmp", number]} Instruction */

/** @type {Instruction[]} */
const input = readFile('inputs/08.txt').split('\r\n').map(line => {
  const stuff = line.split(' ');
  return [stuff[0], parseInt(stuff[1])];
});

/**
 * @param {Instruction[]} instructions
 * @returns {[boolean, number]}
 */
const executor = (instructions) => {
  let position = 0;
  let acc = 0;
  const seen = new Set();
  const functions = {
    nop: () => position++,
    jmp: arg => position += arg,
    acc: arg => {
      acc += arg;
      position++;
    },
  }
  while(position < instructions.length){
    if(seen.has(position) || position < 0) return [false, acc];
    seen.add(position);
    const [instruction, arg] = instructions[position];
    functions[instruction](arg);
  }
  return [true, acc];
}

const toggle = str => str === 'jmp' ? 'nop' : 'jmp';

console.log(executor(input)[1]);

for(let i = 0; i < input.length; i++){
  if(input[i][0] === 'acc') continue;
  input[i][0] = toggle(input[i][0]);
  const [graceful, acc] = executor(input);
  if(graceful) console.log(acc);
  input[i][0] = toggle(input[i][0]);
}
