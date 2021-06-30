import { readFile, group, minBy, countMatches } from '../tools.ts';

const input = readFile('./inputs/08.txt').split('').map(c => parseInt(c));

const layers = group(input, 25 * 6);

const bestLayer = minBy(layers, layer => countMatches(layer, e => e === 0));

console.log(countMatches(bestLayer, e => e === 1) * countMatches(bestLayer, e => e === 2));

const combined = layers.reduce((acc, cur) => acc.map((c, i) => c === 2 ? cur[i] : c))

group(combined, 25).forEach(r => console.log(r.map(c => [' ', '#', '?'][c]).join('')))
