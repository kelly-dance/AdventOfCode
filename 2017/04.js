import { readFile, countMatches } from '../tools.ts';

const input = readFile('./inputs/04.txt').split('\n').map(l => l.split(' '));

console.log(countMatches(input, e => {
  const seen = new Set();
  for(const word of e){
    if(seen.has(word)) return false;
    seen.add(word);
  }
  return true;
}));
console.log(countMatches(input, e => {
  const seen = new Set();
  for(const word of e){
    const sorted = word.split('').sort().join('');
    if(seen.has(sorted)) return false;
    seen.add(sorted);
  }
  return true;
}));
