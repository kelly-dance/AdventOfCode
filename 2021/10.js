import { readAdvent, sum } from '../tools.ts';

const inp = (await readAdvent()).split('\n');

const start = '([{<';
const end = ')]}>';
const pointVals = [3,57,1197,25137];

const scores = inp.map(l => {
  const stack = [];
  for(const char of l.split('')){
    if(start.includes(char)) stack.push(char);
    else if(end.includes(char)){
      const i = end.indexOf(char);
      if(stack.pop() !== start.charAt(i)) return [1, pointVals[i]];
    }
  }
  if(stack.length){
    let score = 0;
    for(const char of stack.reverse()){
      score *= 5;
      score += 1 + start.indexOf(char);
    }
    return [2, score]
  }
  return [0];
});

// Part 1
console.log(sum(scores.filter(([part]) => part === 1).map(([_, score]) => score)))

// Part 2
const p2Scores = scores.filter(([part]) => part === 2).map(([_, score]) => score);
p2Scores.sort((a,b)=>a-b);
console.log(p2Scores[Math.floor(p2Scores.length / 2)])
