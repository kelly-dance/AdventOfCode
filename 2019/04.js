import { readFile, pickIntsFromString, countMatches, range } from '../tools.ts'

const [min, max] = pickIntsFromString(readFile('./inputs/04.txt'));

const p1 = countMatches(range(min, max), n => {
  const asStr = '0' + String(n);
  if(asStr.length !== 7) return false;
  let double = false;
  for(let i = 1; i < 7; i++){
    if(asStr.charAt(i) < asStr.charAt(i-1)) return false;
    if(asStr.charAt(i) === asStr.charAt(i-1)) double = true;
  }
  return double;
})

const p2 = countMatches(range(min, max), n => {
  const asStr = '00' + String(n) + 'a';
  if(asStr.length !== 9) return false;
  let double = false;
  for(let i = 2; i < 8; i++){
    if(asStr.charAt(i) < asStr.charAt(i-1)) return false;
    else if(asStr.charAt(i) === asStr.charAt(i-1) && asStr.charAt(i) !== asStr.charAt(i-2) && asStr.charAt(i) !== asStr.charAt(i+1)) double = true;
  }
  return double;
});

console.log(`Part 1: ${p1}`);
console.log(`Part 2: ${p2}`);
