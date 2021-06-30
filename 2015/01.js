import { readFile, sum } from '../tools.ts';

console.log(sum(readFile('./inputs/01.txt').split('').map(c => ({'(':1,')':-1}[c]))))

const input = readFile('./inputs/01.txt')

let depth = 0;
for(let i = 0; i < input.length; i++){
  const c = input[i];
  if(c === '(') depth++;
  else {
    if(depth === 0) {
      console.log(i + 1)
      break;
    }
    depth--;
  }
}

