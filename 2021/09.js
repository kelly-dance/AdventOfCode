import { invertMap, multiLoop, product, readAdvent, sum, Vec2 } from '../tools.ts';

const inp = (await readAdvent()).split('\n').map(s => {
  return s.split('').map(c=>parseInt(c));
})

const lows = inp.flatMap((r,i) => r.map((v, j) => [v, i, j]).filter(([v, _, j]) => {
  for(const {x,y} of Vec2.SIDES){
    if(inp[i+x]?.[j+y] != undefined && v >= inp[i+x]?.[j+y]) return false;
  }
  return true;
}));

console.log(sum(lows.map(([v]) => v + 1)));

const basins = new Map();
for(const [_,x,y] of lows) basins.set(`${x},${y}`, `${x},${y}`);

const flow = (x,y) => {
  const s = `${x},${y}`;
  if(basins.has(s)) return basins.get(s);
  const v = inp[x][y];
  if(v === 9) return;
  const neighs = Vec2.SIDES.map(vec => [x+vec.x,y+vec.y]).find(([a,b]) => inp[a]?.[b] != undefined && inp[a]?.[b] < v);
  if(!neighs) throw new Error('?');
  const ret = flow(...neighs);
  basins.set(s, ret);
  return ret;
}

for(const [_, x, y] of multiLoop(2, inp)){
  flow(x,y);
}

console.log(product([...invertMap(basins).values()].map(l => l.length).sort((a,b)=>b-a).slice(0,3)));
