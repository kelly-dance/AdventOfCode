import { readFile } from '../tools.ts';

const inp = parseInt(readFile('./inputs/19.txt'))
// part 1
console.log((inp - 2 ** Math.floor(Math.log2(inp))) * 2 + 1)
//part 2
const toSub = 3 ** Math.floor(Math.log(inp)/Math.log(3));
let subbed = 0;
let leftOver = inp;
while(leftOver > toSub){
  subbed++;
  leftOver -= toSub;
}
const prediction = toSub * Math.abs(subbed - 1) + leftOver * subbed;
console.log(prediction)
