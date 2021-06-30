import { inRange, product, range, readFile, sum, zipWithIndex } from '../tools.ts';

const [fields, mine, nearby] = readFile('inputs/16.txt').split('\n\n');


const parsedFields = fields.split('\n').map((l, i) => {
  const name = l.match(/(\w+? ?\w+):/)[1];
  /** @type {[number, number][]} */
  const nums = l.match(/\d+-\d+/g).map(s => s.split('-').map(Number))
  return {
    name,
    ranges: nums,
    index: i,
  }
});

const parsedNearby = nearby
  .split(/\n/)
  .slice(1)
  .map(l => l.split(',').map(Number))

console.log(sum( // PART 1
  parsedNearby.map(ns => sum(
    ns.filter(
      n => !parsedFields.some(({ranges}) => ranges.some(([lo, hi]) => inRange(n, lo, hi, true)))
    )
  ))
))

// Filter out tickets that contain a invalid number
const valid = parsedNearby.filter(ns => !ns.some(n => !parsedFields.some(({ranges}) => ranges.some(([lo, hi]) => inRange(n, lo, hi, true)))))

// compare every field against every column of tickets
const mappings = range(parsedFields.length).map(from => range(parsedFields.length).map(to => 
  valid.every(ns =>  parsedFields[from].ranges.some(([lo, hi]) => inRange(ns[to], lo, hi, true)))
))

// backtracking to find the valid order
const findOrder = (fields, prev = []) => {
  if(fields.length === prev.length) return prev;
  for(const field of fields){
    if(prev.includes(field)) continue;
    if(!mappings[field.index][prev.length]) continue;
    const result = findOrder(fields, [...prev, field]);
    if(result) return result;
  }
} 

const parsedTicket = mine.split('\n')[1].split(',').map(Number)

console.log(product( // PART 2
  zipWithIndex(findOrder(parsedFields))
    .filter(([{name}]) => name.includes('departure'))
    .map(([_, i]) => parsedTicket[i])
));
