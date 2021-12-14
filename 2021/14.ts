import { add, DefaultMap, id, maxBy, minBy, readAdvent, succ } from '../tools.ts';

const [initalPoly, recipesString] = (await readAdvent()).split('\n\n');
const recipes = recipesString.split('\n').map(s => s.split(' -> '));

const recipeMap = new Map();
for(const [template, inserts] of recipes) recipeMap.set(template, inserts);

let polypairs = new DefaultMap<string, number>(() => 0);
for(let i = 0; i < initalPoly.length - 1; i++){
  const pair = initalPoly.substring(i, i + 2);
  polypairs.apply(pair, succ);
}

const printScore = () => {
  let counts = new DefaultMap<string, number>(() => 0);
  for(let [pair, count] of polypairs.entries()) counts.apply(pair[1], add(count));
  
  counts.apply(initalPoly[0], succ);
  
  const max = maxBy(counts.values(), id);
  const min = minBy(counts.values(), id);
  console.log(max - min);
}

for(let i = 0; i < 40; i++){
  if(i === 10) printScore();
  const nextPairs = new DefaultMap<string, number>(() => 0);
  for(const [entry, count] of polypairs.entries()){
    const inserts = recipeMap.get(entry);
    nextPairs.apply(entry[0] + inserts, add(count));
    nextPairs.apply(inserts + entry[1], add(count));
  }
  polypairs = nextPairs;
}
printScore();
