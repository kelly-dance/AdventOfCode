import { countMatches, id, readAdvent, sum, transpose } from '../tools.ts';

const [drawStr, ...boardStrs] = (await readAdvent()).split('\n\n');
const draws: number[] = drawStr.split(',').map(s => parseInt(s));
let boards: [number, boolean][][][] = boardStrs.map(b => {
  return b.split('\n')
      .map(l => {
        return l.trim()
          .split(/\s+/g)
          .map(s => [parseInt(s), false] as [number, boolean])
      })
});

const won: boolean[] = boards.map(() => false); // used to store whether board is won already or not

for(let i = 0; i < draws.length; i++){
  const draw = draws[i];
  boards = boards.map(b => b.map(r => r.map(([num, drawn]) => [num, drawn || num === draw]))); // mark instances of this number in boards as drawn
  for(let j = 0; j < boards.length; j++){
    if(won[j]) continue;
    const b = boards[j];
    if(b.some(r => r.every(e => e[1])) || transpose(b).some(r => r.every(e => e[1]))){ // if a row or a column is all marked
      won[j] = true; // mark it as won
      if(countMatches(won, id) === 1 || won.every(id)){ // if this first or last win
        console.log(sum(b.flatMap(r => r.map(([num, drawn]) => drawn ? 0 : num))) * draw); // output score. flattens 2d array and changes drawn to 0s and sums
      }
    }
  }
}
