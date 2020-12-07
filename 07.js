import { readFile, sum } from './tools.ts';

const input = Object.fromEntries(readFile('inputs/07.txt').split('\r\n').map(line => {
  const bagType = line.match(/(\w+ \w+) bags contain/)[1];
  const bags = line.match(/(\d) (\w+ \w+)/gi)?.map(b => {
    const things = b.split(' ');
    return {
      amount: parseFloat(things[0]),
      name: things[1] + ' ' + things[2],
    }
  }) || [];
  return [bagType,bags]
}));

const seen = new Set();

const exploreUp = bag => {
  if(seen.has(bag)) return;
  seen.add(bag)
  Object.entries(input).forEach(([key, val]) => {
    if(val.some(b => b.name == bag)) exploreUp(key);
  })
}
exploreUp('shiny gold');

console.log(seen.size - 1);

const exploreDown = (bag, multi = 1, cache = {}) => {
  if(input[bag].length === 0) return;
  input[bag].forEach(b => {
    if(!cache[b.name])  cache[b.name] = 0;
    cache[b.name] += multi * b.amount;
    exploreDown(b.name, multi * b.amount, cache);
  })
  return cache;
}
console.log(sum(Object.values(exploreDown('shiny gold'))))

