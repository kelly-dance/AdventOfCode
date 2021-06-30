import { readFile, sum } from '../tools.ts';

const inp = readFile('./inputs/06.txt').split('\n').map(l => l.split(')'));

type Obj = {
  id: string,
  orbits: Obj | null,
  orbitedBy: Obj[]
}

const treeSum = (node: Obj, depth = 0): number => depth + ((node.orbitedBy.length > 0) ? sum(node.orbitedBy.map(n => treeSum(n, depth + 1))) : 0);
const pathToCOM = (n: Obj): string[] => n.orbits ? [n.id, ...pathToCOM(n.orbits)] : [];

const orbitMap: Record<string, Obj> = {};

for(const obj of new Set(inp.flat(1))) orbitMap[obj] = { id: obj, orbits: null, orbitedBy: [] };

for(const [big, smol] of inp){
  orbitMap[smol].orbits = orbitMap[big];
  orbitMap[big].orbitedBy.push(orbitMap[smol]);
}

console.log(treeSum(orbitMap['COM']));

const p1 = pathToCOM(orbitMap['YOU'].orbits as Obj);
const p2 = pathToCOM(orbitMap['SAN'].orbits as Obj);
const ancestor = p1.reduce((acc,key) => acc ? acc : p2.includes(key) ? key : '', '');
console.log(p1.indexOf(ancestor) + p2.indexOf(ancestor));
