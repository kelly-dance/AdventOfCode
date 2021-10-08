import { readFile } from '../tools.ts';
const raw = readFile('./inputs/16.txt');

//Part 1:
let input = raw.split('').map(Number);
for(let i = 0; i < 100; i++){
  let next = new Array(input.length);
  for(let j = 0; j < input.length; j++){
    const gen = phaseGenerator(j+1);
    gen.next();
    next[j] = Math.abs(input.reduce((acc,c) => acc + gen.next().value * c, 0) % 10);
  }
  input = next;
}

function* phaseGenerator(n: number): Generator<-1 | 0 | 1> {
  while(true){
    for(let i = 0; i < n; i++) yield 0;
    for(let i = 0; i < n; i++) yield 1;
    for(let i = 0; i < n; i++) yield 0;
    for(let i = 0; i < n; i++) yield -1;
  }
}
console.log("Part 1: " + input.slice(0, 8).join(''));

//Part 2:
const offset = Number(raw.substring(0,7));
input = raw.repeat(10000).substring(offset).split('').map(Number);
for(let i = 0; i < 100; i++){
  for(let j = input.length-2; j >= 0; j--){
    input[j] = Math.abs(input[j + 1] + input[j]) % 10;
  }
}
console.log("Part 2: " + input.slice(0, 8).join(''));
