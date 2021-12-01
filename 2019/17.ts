import { readFile, zip, Vec2, mapFrom, DefaultMap, group } from '../tools.ts';
import * as int from './intcode.ts';

const program = readFile('./inputs/17.txt').split(',').map(s => parseInt(s));

const dirs = mapFrom(zip('<^>v'.split(''), Vec2.SIDES));

const map: boolean[][] = [];
let cur: boolean[] = [];
let pos: Vec2 = Vec2.ORIGIN;
let dir: Vec2 = Vec2.ORIGIN;
int.scriptManager(int.prepareState(program), function*(send, rec){
  while(true){
    const char = String.fromCharCode(Number(yield rec()));
    if('>v<^'.includes(char)){
      pos = new Vec2(map.length, cur.length);
      dir = dirs.get(char)!;
    }
    if(char === '\n') {
      map.push(cur);
      cur = [];
    } else cur.push(char !== '.');
  }
});

let acc = 0;
for(let i = 1; i < map.length - 1; i++){
  for(let j = 1; j < map[i].length - 1; j++){
    if(map[i][j] && Vec2.SIDES.every(({x, y}) => map[i+x][j+y])) acc += i * j;
  }
}

console.log(acc)

const hor: [Vec2, Vec2][] = [];
const ver: [Vec2, Vec2][] = [];

for(let i = 0; i < map.length; i++){
  for(let j = 0; j < map[i].length; j++){
    if(map[i][j] && map[i][++j]) {
      let s = j-1;
      let e = j;
      while(map[i][j]){
        e = j;
        j++;
      }
      if(s != e) ver.push([new Vec2(i, s), new Vec2(i, e)]);
    }
  }
}

for(let i = 0; i < map[0].length; i++){
  for(let j = 0; j < map.length; j++){
    if(map[j][i] && map[++j][i]) {
      let s = j-1;
      let e = j;
      while(map[j][i]){
        e = j;
        j++;
      }
      if(s != e) hor.push([new Vec2(s, i), new Vec2(e, i)]);
    }
  }
}

const graph = new DefaultMap<`${number},${number}`, Vec2[]>(() => []);
const seen = new Set<string>()
for(const [a, b] of [...hor, ...ver]){
  seen.add([a,b].sort().join(','));
  graph.get(a.toString()).push(b);
  graph.get(b.toString()).push(a);
}

const rotations = Vec2.SIDES.map(v => v.toString());

const hist = [pos];
const steps: (string | number)[] = [];
while(seen.size){
  const cur = hist[0];
  const options = graph.get(cur.toString()).filter(o => seen.has([cur, o].sort().join(',') as any));
  if(options.length !== 1) throw new Error('what?')
  const target = options[0];
  const delta = cur.sub(target);
  const targetDir = delta.normalized();
  const angleDif = (4 + rotations.indexOf(dir.toString()) - rotations.indexOf(targetDir.toString())) % 4;
  dir = targetDir;
  switch(angleDif){
    case 1:
      steps.push('L');
      break;
    case 2:
      throw new Error('I think this should be unused')
    case 3:
      steps.push('R');
      break;
  }
  const dist = delta.manhattenLen();
  if(typeof steps[steps.length - 1] === 'number') (steps[steps.length - 1] as number) += dist;
  else steps.push(dist);
  hist.unshift(target)
  seen.delete([cur, target].sort().join(','));
}

const replaceIn = <T, R>(search: T[], replace: R, arr: (T | R)[]): number => {
  for(let i = 0; i < arr.length; i++){
    if(search.every((e, j) => e === arr[i + j])) {
      arr.splice(i, search.length - 1);
      arr[i] = replace;
      return 1 + replaceIn(search, replace, arr);
    }
  }
  return 0;
}

const path = group(steps, 2).map(([t, d]) => `${t},${d}`);

const [main, A, B, C] = (()=>{
  const as = 0;
  for(let ae = as + 1; ae < path.length; ae++){
    const Aentries = path.slice(as, ae);
    const Astring = Aentries.join(',');
    if(Astring.length > 20) continue;
    const Acopy = [...path];
    const replaced = replaceIn(Aentries, 'A', Acopy);
    if(replaced <= 1) continue;
    for(let bs = ae; bs < path.length; bs++){
      for(let be = bs + 1; be < path.length; be++){
        const Bentries = path.slice(bs, be);
        const Bstring = Bentries.join(',');
        if(Bstring.length > 20) continue;
        const Bcopy = [...Acopy];
        const replaced = replaceIn(Bentries, 'B', Bcopy);
        if(replaced <= 1) continue;
        for(let cs = be; cs < path.length; cs++){
          for(let ce = cs + 1; ce < path.length; ce++){
            const Centries = path.slice(cs, ce);
            const Cstring = Centries.join(',');
            if(Cstring.length > 20) continue;
            const Ccopy = [...Bcopy];
            const replaced = replaceIn(Centries, 'C', Ccopy);
            if(replaced <= 1) continue;
            if(Ccopy.every(s => 'ABC'.includes(s))) {
              return [Ccopy.join(','), Astring, Bstring, Cstring]
            }
          }
        }
      }
    }
  }
  throw new Error('no sln found')
})();

const script = `${main}\n${A}\n${B}\n${C}\nn\n`
let i = 0;

program[0] = 2;
int.scriptManager(int.prepareState(program), function*(send, rec, type){
  while(true){
    switch(yield type()){
      case int.ScriptIOMode.receive:
        const val = yield rec();
        if(val > 1000) console.log(Number(val));
        break
      case int.ScriptIOMode.provide:
        yield send(BigInt(script[i].charCodeAt(0)));
        i++;
        break;
    }
  }
});
