import { memoize, readFile } from './tools.ts';

const input = readFile('inputs/10.txt').split('\r\n').map(Number).sort((a,b)=>a-b);
input.push(input[input.length - 1] + 3);

let ones = 0;
let threes = 0;

let prev = 0;
for(const next of input){
  if(next - prev === 1) ones++;
  if(next - prev === 3) threes++;
  prev = next;
}

console.log(ones * threes);

input.unshift(0);

const from = memoize(index => {
  if(index === input.length - 1) return 1;
  const n = input[index];
  let score = 0;
  for(let i = index + 1; input[i] - n <= 3 && i < input.length; i++) score += from(i);
  return score;
});
console.log(from(0))
