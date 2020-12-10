import { memoize, product, readFile, zip } from './tools.ts';

const input = readFile('inputs/10.txt').split('\r\n').map(Number).sort((a, b) => a - b);
input.unshift(0);
input.push(input[input.length - 1] + 3);

let ones = 0;
let threes = 0;

for(const [prev, curr] of zip(input, input.slice(1))){
  if(curr - prev === 1) ones++;
  if(curr - prev === 3) threes++;
}

console.log(ones * threes);

const from = memoize(index => {
  if(index === input.length - 1) return 1;
  const n = input[index];
  let score = 0;
  for(let i = index + 1; input[i] - n <= 3 && i < input.length; i++) score += from(i);
  return score;
});

console.log(from(0));

// Wack code based on https://oeis.org/A000073
const a = (19+3*Math.sqrt(33))**(1/3);
const b = (19-3*Math.sqrt(33))**(1/3);
const formula = n => Math.round(3*((a+b+1)/3)**(n+1)/(a**2+b**2+4));

const straights = [];
let straight = 1;
for(const [prev, curr] of zip(input, input.slice(1))){
  if(curr - prev === 1) straight++;
  else { // curr - prev === 3 
    straights.push(straight)
    straight = 1;
  }
}

console.log(product(straights.map(formula)))
