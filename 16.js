import { first, id, inRange, mapGenerator, product, range, readFile, sum, takeAll, zipWithIndex } from './tools.ts';

const [fields, mine, nearby] = readFile('inputs/16.txt').split('\r\n\r\n');


const parsedFields = fields.split('\r\n').map((l, i) => {
  const name = l.match(/(\w+? ?\w+):/)[1];
  /** @type {[number, number][]} */
  const nums = l.match(/\d+-\d+/g).map(s => s.split('-').map(Number))
  return {
    name,
    ranges: nums,
    index: i,
  }
});

const parsedNearby = nearby.split(/\r\n/).slice(1).map(l => l.split(',').map(Number))

console.log(sum(parsedNearby.map(ns => sum(ns.filter(n => !parsedFields.some(({ranges}) => ranges.some(([lo, hi]) => inRange(n, lo, hi, true))))))))

const valid = parsedNearby.filter(ns => !ns.some(n => !parsedFields.some(({ranges}) => ranges.some(([lo, hi]) => inRange(n, lo, hi, true)))))

const mappings = Array.from({length: parsedFields.length}, (_, f) => Array.from({length: parsedFields.length}, (_, t) => {
  return valid.every(ns =>  parsedFields[f].ranges.some(([lo, hi]) => inRange(ns[t], lo, hi, true)));
}))

const findOrder = (fields, prev = []) => {
  if(fields.length === prev.length) return prev;
  for(const field of fields){
    if(prev.includes(field)) continue;
    if(!mappings[field.index][prev.length]) continue;
    const result = findOrder(fields, [...prev, field]);
    if(result) return result;
  }
} 

const parsedTicket = mine.split('\r\n')[1].split(',').map(Number)

const order = findOrder(parsedFields);

console.log(product(zipWithIndex(order).filter(([{name}]) => name.includes('departure')).map(([_, i]) => parsedTicket[i])));
