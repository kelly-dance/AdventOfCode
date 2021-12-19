import { readAdvent } from '../tools.ts';

type parsed = number | [parsed, parsed];

const inp: parsed[] = (await readAdvent()).split('\n').map(s => JSON.parse(s));

const copy = (p: parsed): parsed => {
  if(typeof p === 'number') return p;
  return p.map(p => copy(p)) as parsed;
}

type action = {
  addLeft: number,
} | {
  addRight: number,
} | {
  kill: boolean,
}

const reduceExplodes = (p: parsed, depth: number): action[] | undefined => {
  if(typeof p === 'number') {
    return undefined;
  };
  if(depth === 4){
    if(typeof p[0] !== 'number' || typeof p[1] !== 'number') throw new Error('never');
    return [{addLeft:p[0]},{addRight:p[1]},{kill: true}]
  }
  const lefta = reduceExplodes(p[0], depth + 1);
  if(lefta){
    const rem: action[] = [];
    for(const action of lefta){
      if(typeof action === 'string') rem.push(action);
      else if('kill' in action) {
        p[0] = 0;
      } else if('addLeft' in action){
        rem.push(action);
      } else if ('addRight' in action){
        const addRightHelper = (a: parsed[]) => {
          if(typeof a[0] === 'number') a[0] += action.addRight;
          else addRightHelper(a[0]);
        }
        if(typeof p[1] == 'number') p[1] += action.addRight;
        else addRightHelper(p[1]);
      }
    }
    return rem;
  }
  const righta = reduceExplodes(p[1], depth + 1);
  if(righta){
    const rem: action[] = [];
    for(const action of righta){
      if(typeof action === 'string') rem.push(action);
      else if('kill' in action) {
        p[1] = 0;
      } else if('addLeft' in action){
        const addLeftHelper = (a: parsed[]) => {
          if(typeof a[1] === 'number') a[1] += action.addLeft;
          else addLeftHelper(a[1]);
        }
        if(typeof p[0] == 'number') p[0] += action.addLeft;
        else addLeftHelper(p[0]);
      } else if ('addRight' in action){
        rem.push(action);
      }
    }
    return rem;
  }
}

const reduceSplits = (p: parsed): [number, number] | undefined | true => {
  if(typeof p === 'number') {
    if(p >= 10) return [Math.floor(p / 2), Math.ceil(p / 2)];
    return undefined;
  }
  const lefta = reduceSplits(p[0]);
  if(lefta){
    if(Array.isArray(lefta)) p[0] = lefta;
    return true;
  }
  const righta = reduceSplits(p[1]);
  if(righta){
    if(Array.isArray(righta)) p[1] = righta;
    return true;
  }
}

const reduce = (p: parsed): boolean => {
  const exploded = reduceExplodes(p, 0);
  if(exploded) return true;
  const split = reduceSplits(p);
  return !!split;
}

let acc = copy(inp[0]);
while(reduce(acc));
for(let i = 1; i < inp.length; i++){
  acc = [acc, copy(inp[i])];
  while(reduce(acc));
}

const evalp = (p: parsed): number => {
  if(typeof p === 'number') return p;
  return 3 * evalp(p[0]) + 2 * evalp(p[1])
}

console.log(evalp(acc));

let max = 0;
for(let i = 0; i < inp.length; i++){
  for(let j = 0; j < inp.length; j++){
    if(i === j) continue;
    const acc: parsed = [copy(inp[i]), copy(inp[j])]
    while(reduce(acc));
    const v = evalp(acc);
    if(v > max) max = v;
  }
}
console.log(max);
