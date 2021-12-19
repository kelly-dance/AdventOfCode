import { DefaultOMap, OSet, pairs, pickIntsFromString, range, readAdvent, repeatedAp, succ, Vec3 } from '../tools.ts';

// I never bother to consider what each of these actually rotate around. I just pretend this:
const rotate1 = ({x,y,z}: Vec3) => new Vec3(-y, x, z) // rotate around z
const rotate2 = ({x,y,z}: Vec3) => new Vec3(-z, y, x) // rotate around x
const rotate3 = ({x,y,z}: Vec3) => new Vec3(x, -z, y) // rotate around y

const all = (p: Vec3): Vec3[] => [ // initial state. red facing forward, white up.
  ...range(4).map(c => repeatedAp(rotate1, c, p)), // white up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate2(p))), // orange up up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate2(rotate2(p)))), // yellow up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate2(rotate2(rotate2(p))))), // red up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate3(p))), // blue up
  ...range(4).map(c => repeatedAp(rotate1, c, rotate3(rotate3(rotate3(p))))), // green up
]


const inp: Vec3[][][] = (await readAdvent()).split('\n\n').map(s => {
  const coords: Vec3[] = s.split('\n').slice(1)
                          .map(l => new Vec3(...pickIntsFromString(l) as [number, number, number]));
  return coords.map(c => all(c));
});

// checks if two scanners are shared, if yes, returns the rotation of the second one and the offset
const check = (a: number, b: number, sourcer: number): undefined | [number, Vec3] => {
  for(let r = 0; r < 24; r++){ // check every possible rotation
    // assuming two scanners are aligned. subtracting a the vector to a given beacon from the
    // vector to that beacon from the other scanner will give the vector from the first
    // scanner to the second.
    // just looping over all pairs and seeing if any canidate offset has enough entries
    // to possibly be valid. idk if this works for every input, but it works for mine :)
    const offsets = new DefaultOMap<Vec3, number>(v => v.toString(), () => 0);
    for(const [i,j] of pairs(range(inp[a].length), range(inp[b].length))){
      offsets.apply(inp[a][i][sourcer].sub(inp[b][j][r]), succ);
    }
    const offset = [...offsets.entries()].find(([_, count]) => count >= 12);
    if(offset) return [r, offset[0]];
  }
}

const pts = new OSet<Vec3>(v => v.toString());
for(let i = 0; i < inp[0].length; i++) pts.add(inp[0][i][0]);
const offsets = new Array<Vec3 | undefined>(inp.length); // offsets from scanner 0 to each
offsets[0] = Vec3.ORIGIN;

// recursive fn to find pairs of scanners that match and build off them to find all
const explore = (scanner: number, startRotation: number) => {
  for(let i = 0; i < inp.length; i++){
    if(offsets[i]) continue;
    const res = check(scanner, i, startRotation);
    if(!res) continue;
    const [r, loc] = res;
    const ownOffset = loc.add(offsets[scanner]!);
    offsets[i] = ownOffset;
    for(let j = 0; j < inp[i].length; j++) pts.add(inp[i][j][r].add(ownOffset));
    explore(i, r);
  }
}

explore(0, 0);

console.log(pts.size);

let max = 0;
for(const [a,b] of pairs(offsets, offsets)){
  const score =  a!.sub(b!).manhattenLen();
  if(score > max) max = score;
}

console.log(max);
