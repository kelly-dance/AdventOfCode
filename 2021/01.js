import { readFile } from '../tools.ts';

const input = readFile('./inputs/01.txt').split('\n').map(s => Number(s));

let prev = input[0]
let count = 0;
for(const val of input.slice(1)){
  if(val > prev) count++;
  prev = val;
}
console.log(count)

let count2 = 0;
let prev2 = input[0] + input[1] + input[2];
for(let i = 3; i < input.length; i++){
  const newVal = prev2 - input[i-3] + input[i];
  if(newVal > prev2) count2++;
  prev2 = newVal;
}
console.log(count2)