import { readFile, pickIntsFromString } from '../tools.ts';

const input = readFile('./inputs/15.txt').split('\n').map(l => pickIntsFromString(l)[0]);

// part 1
(()=>{
  let [a, b] = input;
  let count = 0;

  for(let i = 0; i < 40_000_000; i++){
    a = (a * 16807) % 2147483647;
    b = (b * 48271) % 2147483647;
    if((a & 0xFFFF) === (b & 0xFFFF)) count++;
  }
  
  console.log(count)
})();

// part 2
(()=>{
  let [a, b] = input;
  let count = 0;

  for(let i = 0; i < 5_000_000; i++){
    do {
      a = (a * 16807) % 2147483647;
    }while(a % 4 !== 0)
    do {
      b = (b * 48271) % 2147483647;
    }while(b % 8 !== 0)
    if((a & 0xFFFF) === (b & 0xFFFF)) count++;
  }
  
  console.log(count)
})();
