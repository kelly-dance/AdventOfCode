import { mod, productBigInt, readFile, sumBigInt, zip } from './tools.ts';

const [mytime, busses] = readFile('inputs/13.txt').split('\r\n');
const goal = Number(mytime)
const times = busses.split(',').filter(s => s !== 'x').map(Number).map(n => [n, n - goal % n]);

const [a,b] = times.reduce(([bi, bs], [ci, cs]) => cs < bs ? [ci, cs] : [bi, bs]);
console.log(a*b)

const basic = busses.split(',').map((s,i) => s === 'x' ? false : [Number(s), i]).filter(Boolean);

const inv = (a, m) => {
  let m0 = m 
  let x0 = 0n
  let x1 = 1n
  if (m === 1n)  return 0n
  while (a > 1n) {
    let q = a / m
    let t = m 
    m = a % m 
    a = t 
    t = x0 
    x0 = x1 - q * x0 
    x1 = t 
  }
  if (x1 < 0n) x1 = x1 + m0;
  return x1 
}

const CRT = (num, rem) => {
  const prod = productBigInt(num);
  return sumBigInt(zip(rem,num).map(([r,n]) => {
    const pp = prod / n
    return r * inv(pp, n) * pp 
  })) % prod;
}

const num = basic.map(([n, _]) => n).map(BigInt);
const rem = basic.map(([n, i]) => mod(n - i,n)).map(BigInt);
console.log(zip(num, rem))
console.log(CRT(num, rem))
