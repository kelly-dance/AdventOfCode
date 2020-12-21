import { readFile, DefaultMap, intersection, takeAll, takeFirst, subtractSets, union, second, succ } from './tools.ts';

/** @type {DefaultMap<string, number>} */
const ings = new DefaultMap(() => 0);

/** @type {[string[], string[]][]} */
const input = readFile('./inputs/21.txt').split('\r\n').map(l => {
  const [ingRaw, alrgRaw] = l.split(' (contains ');
  const ingSplited = ingRaw.split(' ');
  const alrgSplited = alrgRaw.split('').slice(0, -1).join('').split(', ');
  ingSplited.forEach(i => ings.apply(i, succ));
  return [ingSplited, alrgSplited]
});

/** @type {Map<string, Set<string>>} */
const map = new Map();

for(const [ings, alrgs] of input){
  for(const alrg of alrgs){
    const already = map.get(alrg);
    if(!already) map.set(alrg, new Set(ings))
    else map.set(alrg, intersection(already, new Set(ings)))
  }
}

while(true){
  let atOne = [];
  let notAtOne = [];
  for(const set of map.values()){
    if(set.size === 1) atOne.push(set);
    else notAtOne.push(set);
  }
  if(!notAtOne.length) break;
  const atOneSet = union(...atOne);
  for(const set of notAtOne) subtractSets(set, atOneSet, true);
}

let tot = 0;
const asArr = takeAll(map.values());
for(const [ing, count] of ings.entries()){
  if(asArr.every(s => !s.has(ing))) tot += count;
}
console.log(tot)

console.log(
  takeAll(map.entries())
    .map(([k, s]) => [k, takeFirst(s.values())])
    .sort((a,b) => a[0].localeCompare(b[0]))
    .map(second)
    .join(',')
)
