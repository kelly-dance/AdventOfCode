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
  return [bagType, bags]
}));

const exploreUp = (bag, cache = new Set()) => cache.has(bag)
  ? cache
  : Object.entries(input)
      .filter(([_, val]) => val.some(b => b.name == bag))
      .forEach(([key, _]) => exploreUp(key, cache)) || cache.add(bag);

console.log(exploreUp('shiny gold').size - 1);

const exploreDown = bag => (input[bag].length === 0) ? 0 : sum(input[bag].map(b => b.amount + b.amount * exploreDown(b.name)));

console.log(exploreDown('shiny gold'))
