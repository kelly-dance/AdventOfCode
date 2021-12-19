import { DefaultOMap, OSet, pairs, pickIntsFromString, range, readAdvent, repeatedAp, succ } from '../tools.ts';

const add = (a: number[], b: number[]) => a.map((v,i) => v + b[i]);
const sub = (a: number[], b: number[]) => a.map((v,i) => v - b[i]);

// I never bother to consider what each of these actually rotate around. I just pretend this:
const rotate1 = ([x,y,z]: number[]) => [-y, x, z] // rotate around z
const rotate2 = ([x,y,z]: number[]) => [-z, y, x] // rotate around x
const rotate3 = ([x,y,z]: number[]) => [x, -z, y] // rotate around y

const all = (p: number[]) => [ // initial state. red facing forward, white up.
  ...range(4).map(c => repeatedAp(rotate1, c, p)), // white up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate2(p))), // orange up up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate2(rotate2(p)))), // yellow up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate2(rotate2(rotate2(p))))), // red up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate3(p))), // blue up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate3(rotate3(rotate3(p))))), // green up
]


const inp: number[][][][] = (await readAdvent()).split('\n\n').map(s => {
  const [_, ...coords] = s.split('\n').map(l => pickIntsFromString(l));
  return coords.map(c => all(c)) as number[][][];
});

// checks if two scanners are shared, if yes, returns the rotation of the second one and the offset
const check = (a: number, b: number, sourcer: number): undefined | [number, number[]] => {
  for(let r = 0; r < 24; r++){ // check every possible rotation
    // assuming two scanners are aligned. subtracting a the vector to a given beacon from the
    // vector to that beacon from the other scanner will give the vector from the first
    // scanner to the second.
    // just looping over all pairs and seeing if any canidate offset has enough entries
    // to possibly be valid. idk if this works for every input, but it works for mine :)
    const offsets = new DefaultOMap<number[], number>(k => k.join(','), () => 0);
    for(const [i,j] of pairs(range(inp[a].length), range(inp[b].length))){
      offsets.apply(sub(inp[a][i][sourcer], inp[b][j][r]), succ);
    }
    const offset = [...offsets.entries()].find(([_, count]) => count >= 12);
    if(offset) return [r, offset[0]];
  }
}

const pts = new OSet<number[]>(a => a.join(','));
for(let i = 0; i < inp[0].length; i++) pts.add(inp[0][i][0]);
const offsets = new Array<number[] | undefined>(inp.length); // offsets from scanner 0 to each
offsets[0] = [0, 0, 0];

// recursive fn to find pairs of scanners that match and build off them to find all
const explore = (scanner: number, startRotation: number) => {
  for(let i = 0; i < inp.length; i++){
    if(offsets[i]) continue;
    const res = check(scanner, i, startRotation);
    if(!res) continue;
    const [r, loc] = res;
    const ownff = add(loc, offsets[scanner]!);
    offsets[i] = ownff
    for(let j = 0; j < inp[i].length; j++) pts.add(add(inp[i][j][r], ownff));
    explore(i, r);
  }
}

explore(0, 0);

console.log(pts.size);

let max = 0;
for(const [a,b] of pairs(offsets, offsets)){
  const score = Math.abs(a![0] - b![0]) + Math.abs(a![1] - b![1]) + Math.abs(a![2] - b![2]);
  if(score > max) max = score;
}

console.log(max);
