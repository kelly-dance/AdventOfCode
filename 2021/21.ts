import * as t from '../tools.ts';


let [p1,p2] = (await t.readAdvent()).split('\n').map(s => {
  const v = t.pickIntsFromString(s);

  return v[1];
});

let c = 0;
let dice = 1;
const roll = () => {
  let r = 0;
  for(let i = 0; i < 3; i++){
    r+=dice;
    c++;
    dice = (c % 100) + 1;
  }
  return r;
}

let [p1p1, p1p2] = [p1, p2]
let [p1s,p2s] =[0,0]
while(true){
  p1p1 = ((p1p1 - 1 + roll()) % 10) + 1;
  p1s += p1p1;
  if(p1s >= 1000) break;
  p1p2 = ((p1p2 - 1 + roll()) % 10) + 1;
  p2s += p1p2;
  if(p2s >= 1000) break;
}

console.log(Math.min(p1s, p2s) * c);

const simulate = t.memoizeMulti((p1: number, p1s: number, p2: number, p2s: number, isp1: boolean): [number, number] => {
  if(p1s >= 21) return [1,0];
  if(p2s >= 21) return [0,1];
  let [a,b] = [0,0]
  for(const rolls of t.pairs(t.range(1,4), t.range(1,4), t.range(1,4))){
    const r = t.sum(rolls);
    let [cp1, cp1s, cp2, cp2s] = [p1,p1s,p2,p2s]
    if(isp1){
      cp1 = ((p1 + r - 1) % 10) + 1
      cp1s += cp1;
    }else{
      cp2 = ((p2 + r - 1) % 10) + 1
      cp2s += cp2;
    }
    const [sa,sb] = simulate(cp1, cp1s, cp2, cp2s, !isp1);
    a += sa;
    b += sb;
  }
  return [a,b];
})

console.log(Math.max(...simulate(p1,0,p2,0,true)));
