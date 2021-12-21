import * as t from '../tools.ts';


const [progi, imgi] = (await t.readAdvent()).split('\n\n')
const img = imgi.split('\n').map(s => {
  // const v = parseInt(s);
  const v = s.split('').map(c => c === '#');
  // const v = s.split('')
  // const v = t.pickIntsFromString(s);

  return v;
});
const initout = new t.OSet<[number, number]>(p => p.toString())
const m = new t.OSet<[number, number]>(p => p.toString())
for(const [v,x,y] of t.multiLoop(2, img)){
  initout.add([y,x]);
  if(v) m.add([y,x]);
}
const prog = progi.split('').map(c => c === '#')

const dothing = (s : t.OSet<[number, number]>, out: t.OSet<[number, number]>, outstate: boolean): [t.OSet<[number, number]>, t.OSet<[number, number]>, boolean] => {
  const next = new t.OSet<[number, number]>(p => p.toString());
  const done = new t.OSet<[number, number]>(p => p.toString());
  for(const [x,y] of out){
    for(let io = -1; io <= 1; io++){
      for(let jo = -1; jo <= 1; jo++){
        const [px, py] = [x+io,y+jo]
        if(done.has([px,py])) continue;
        done.add([px,py])
        let n = 0;
        
        for(let i = -1; i <= 1; i++){
          for(let j = -1; j <= 1; j++){
            n *= 2;
            if(s.has([px+i,py+j]) || (!out.has([px+i,py+j]) ? outstate : false)) n++;
          }
        }
        if(prog[n]) next.add([px,py]);
      }
    }
    
  }
return [next, done, prog[0] ? (outstate ? prog[511] : prog[0]) : false];
}

const print = (m: t.OSet<[number, number]>, out:t.OSet<[number, number]>, outstate: boolean, ...args: any[]) => {
  for(let i = -3; i <= 25; i++){
    for(let j = -3; j <= 25; j++){
      t.printChar((m.has([i,j])) ? '#' : ((!out.has([i,j]) ? (outstate ? 'O' : 'o') : '.')))
    }
    t.printChar('\n')
  }
  console.log(...args)
}

let [s,o,st] = [m,initout,false];
for(let i = 0; i < 50; i++){
  if(i === 2) console.log(s.size);
  [s,o,st] = dothing(s,o,st);
}
console.log(s.size)


// 5396
// 5438