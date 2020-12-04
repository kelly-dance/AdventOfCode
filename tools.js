export const Digits = [0,1,2,3,4,5,6,7,8,9];

/**
 * [start, end)
 * if only 1 param is passed start is 0 and param is end
 * increment defaults to 1
 * @param {number} start 
 * @param {number=} end
 * @param {number=} inc
 * @returns {number[]} 
 */
export const range = (start, end, inc = 1) => {
  if(!end){
    end = start;
    start = 0;
  }
  return Array.from(
    {
      length: Math.ceil((end - start) / inc)
    },
    (_, i) => i * inc + start
  );
}

export const newSieve = max => 
  range(2,Math.ceil(Math.sqrt(max+1)))
    .reduce(
      (s, n) =>
        s.includes(n) ? 
          s.filter(x => x < n ** 2 || x % n !== 0) : 
          s,
      range(2,max+1)
    )

/**
 * Generates primes less than max
 * @param {number} max 
 * @returns {number[]} 
 */
export function Sieve(max) {
  let arr = Bools(max);
  const count = arr.reduce((acc,el) => el ? acc + 1 : acc, 0); // bc having to make new resized array is slow
  const primes = new Array(count);
  for(let i = 0, j = 0; i < arr.length; i++) if(arr[i]) primes[j++] = i;
  return primes;
}

/**
 * Sieve
 * @param {number} max 
 * @returns {Boolean[]} true is index # is prime
 */
export function Bools(max){
  let arr = new Array(max).fill(true);
  arr[0]=arr[1]=false;
  for(let i = 2; i <= Math.sqrt(max); i++){
    if(arr[i]) for(let j = i*i; j < max; j+=i) arr[j] = false;
  }
  return arr;
}

export function Mod(n,t){
  if(n>=0) return n%t;
  else return t+(n%t);
}

export function SudokuSolve(b){
  function solve(b){
    let x, y = -1;
    for(x = 0; x < 9 && y == -1; x++){
      y = b[x].indexOf(0);
    } x--;
    if(x == 8 && y == -1) return b;
    else {
      let cs = SudokuChoices(b,x,y);
      cs.forEach(c=>{
        let nb = b.map(arr=>arr.slice());
        nb[x][y]=c;
        let sol = solve(nb);
        if(sol) return sol;
      });
      return false;
    }
  }
  return solve(b);
}

export function SudokuRandom(b){
  if (!b) b = new Array(9).fill(new Array(9).fill(0));
  let prog = 81;
  function solve(b){
    let p = [];
    for(xPos in b){
      for(yPos in b[xPos]){
        if(b[xPos][yPos]==0) p.push({x: xPos, y: yPos});
      }
    }
    if(p.length < 61 && Equals2dArr(SudokuSolve(b),SudokuSolve(b,true))) return b;
    else {
      let pos = p[Math.floor(Math.random()*p.length)];
      let cs = ShuffleArray(SudokuChoices(b,pos.x,pos.y));
      if(p.length < prog){
        prog = p.length;
        console.log(b);
        console.log(pos.x, pos.y, cs, p.length);
      }
      for(let c of cs){
        let nb = b.map(arr=>arr.slice());
        nb[pos.x][pos.y]=c;
        let sol = solve(nb);
        if(sol) return sol;
      };
      return false;
    }
  }
  return solve(b);
}

export function Equals2dArr(arr1, arr2){
  if(arr1.length == arr2.length){
    for(let x = 0; x < arr1.length; x++){
      for(let y = 0; y < arr1[x].length; y++){
        if(arr1[x][y]!=arr2[x][y]) return false;
      }
    } return true;
  } return false;
}

export function SudokuChoices(arr, x , y){
  let seen = new Set(Digits.slice(1));
  for(let i = 0; i < 9; i++){
    seen.delete(arr[x][i]);
    seen.delete(arr[i][y]);
    seen.delete(arr[Math.floor(i/3)+3*Math.floor(x/3)][(i%3)+3*Math.floor(y/3)]);
  }
  return [...seen];
}

export function ShuffleArray(arr){
  let array = arr.slice();
  for(let i = array.length - 1; i > 0; i--){
    const j = Math.floor(Math.random() * i)
    const temp = array[i]
    array[i] = array[j]
    array[j] = temp
  }
  return array;
}

/**
 * @param {(bigint[] | number[])} nums 
 */
export const sum = nums => nums.reduce((a, n) => a + n, typeof nums[0] === 'bigint' ? 0n : 0);

/**
 * @param {(bigint[] | number[])} nums 
 */
export const product = nums => nums.reduce((a, n) => a * n, typeof nums[0] === 'bigint' ? 1n : 1);

/**
 * PLEASE PASS PRIMES TO THIS OR MEGA SLOW
 * @param {number} n 
 * @param {number[]} primes 
 * @returns {number[]}
 */
export const primeFactors = (n, primes = null) => {
  if(primes === null) primes = Sieve(n);
  let runningTotal = n;
  const factors = [];
  let i = 0;
  while(runningTotal !== 1 && i < primes.length){
    if(runningTotal % primes[i] === 0){
      factors.push(primes[i]);
      runningTotal /= primes[i];
    }
    else i++;
  }
  if(runningTotal !== 1) factors.push(runningTotal);
  return factors;
}

/**
 * @template T
 * @param {T[]} arr 
 * @returns {Map<T, number>}
 */
export const countRepeats = arr => {
  const map = new Map();
  for(const elem of arr){
    if(!map.has(elem)) map.set(elem, 1);
    else map.set(elem, map.get(elem) + 1);
  }
  return map;
}

/**
 * @param {number} a 
 * @param {number} b 
 */
export const intDiv = (a,b) => Math.floor(a/Number(b));

/**
 * @param {number} n
 * @param {number=} pad
 * @returns {number[]} 
 */
export const digitsOf = (n, pad = null) => {
  if(n<0) throw new Error('Invalid argument, number must be positive');
  const digits = [];
  while(n > 0){
    digits.push(n%10);
    n = intDiv(n,10);
  }
  if(pad !== null){
    while(digits.length < pad) digits.push(0)
  }
  return digits;
}

/**
 * @param {bigint} n
 * @param {number=} pad
 * @returns {bigint[]} 
 */
export const digitsOfBigInt = (n, pad = null) => {
  if(n < 0) throw new Error('Invalid argument, number must be positive');
  const digits = [];
  while(n > 0){
    digits.push(n % 10n);
    n /= 10n;
  }
  if(pad !== null){
    while(digits.length < pad) digits.push(0)
  }
  return digits;
}

/**
 * @param {number | bigint} n 
 * @returns {boolean}
 */
export const isPalindrome = n => {
  const digits = typeof n === 'number' ? digitsOf(n) : digitsOfBigInt(n);
  for(let i = 0; i < digits.length / 2; i++) if(digits[i] !== digits[digits.length - i - 1]) return false;
  return true;
}

/**
 * @param {number[]} digits 
 * @returns {number}
 */
export const numberFromDigits = digits => {
  return sum(digits.map((n,i)=>n*10**i));
}

/**
 * @param {bigint[]} digits 
 * @returns {bigint}
 */
export const bigIntFromDigits = digits => {
  return sum(digits.map((n,i)=>n*10n**BigInt(i)));
}

/**
 * @param {number} n 
 */
export const reverseNumber = n => numberFromDigits(digitsOf(n).reverse());

/**
 * @param {bigint} n 
 */
export const reverseBigInt = n => bigIntFromDigits(digitsOfBigInt(n).reverse());

/**
 * @template T
 * @param {T[]} arr 
 * @param {number} times 
 * @returns {Generator<T[]>}
 */
export const combos = function*(arr, times){
  if(times < 1 || Math.floor(times) !== times) return;
  if(times===1){
    for(const elem of arr) yield [elem];
  }else{
    for(const elem of arr){
      const lowerGen = combos(arr, times-1);
      for(const rest of lowerGen){
        yield [elem, ...rest];
      }
    }
  }
}

/**
 * @template T
 * @param {T[]} arr 
 * @param {number} times 
 * @returns {Generator<T[]>}
 */
export const combosWithLowerTimes = function*(arr, times){
  for(let i = times; i > 0; i--){
    yield* combos(arr,i);
  }
}

/**
 * @template P
 * @template R
 * @param {(arg0: P) => R} fn
 * @param {Map<P, R>=} defaults
 * @returns {(arg0: P) => R} 
 */
export const memoize = (fn, defaults) => {
  /**
   * @type {Map<P,R>}
   */
  const cache = defaults || new Map();
  return arg => {
    if(cache.has(arg)) return cache.get(arg);
    const result = fn(arg);
    cache.set(arg, result);
    return result;
  }
}

/**
 * @param {number} n
 * @returns {number[]}
 */
export const factors = memoize(n => {
  const factors = [1];
  for(let i = 2; i < Math.ceil(Math.sqrt(n)); i++){
    if(Number.isInteger(n/i)) factors.push(i, n/i);
  }
  return factors;
})

/**
 * returns array with the same length as the a array
 * @template S
 * @template T
 * @param {S[]} a 
 * @param {T[]} b 
 * @returns {[S,T][]}
 */
export const zip = (a,b) => a.map((v,i) => [v, b[i]]);

/**
 * @param {string} path 
 * @returns {string}
 */
export const readFile = path => (new TextDecoder("utf-8")).decode(Deno.readFileSync(path));

/**
 * @param {number} a 
 * @param {number} b 
 * @returns {number}
 */
export const isCoprime = (a,b) => {
  for(let i = 2; i <= Math.min(a, b); i++) if(b % i === 0 && a % i === 0) return false;
  return true;
}

/**
 * @param {number} n 
 */
export const totient = n => {
  let counter = 0;
  for(const num of range(1,n)) {
    if(isCoprime(n, num)) counter++;
  }
  return counter;
}

/**
 * Highly recomended to pass an array of primes to this function.
 * Must reach atleast as high as n
 * @param {number} n 
 * @param {number[]=} primes
 */
export const newTotient = (n, primes) => {
  if(!primes) primes = Sieve(n);
  let t = n;
  for(const p of primes){
    if(n % p === 0) t *= 1 - 1/p;
  }
  if(t === n) t--;
  return Math.round(t);
}

/**
 * @param {number} target 
 */
export const totientsUpTo = target => {
  const phi = range(0,target+1);
  for(let n = 2; n <= target; n++){
      if(phi[n] == n){ // this number is prime
        phi[n]--; // phi(prime) = prime - 1
        for(let i = n*2; i <= target; i += n) // loop through multiples of this number
          phi[i] *= 1 - (1 / n); //removes the stuff
      }
  }
  return phi;
}

/**
 * @template T
 * @template R
 * @param {string} label
 * @returns {(fn: (...args: T) => R) => (...args: T) => R}
 */
export const debugDecorator = label => fn => (...args) => {
  console.log(`called`, label, 'args', args)
  const result = fn(...args)
  console.log(`returned from`, label, result, 'args', args)
  return result;
}

/**
 * @param {number} n 
 */
export const factorial = n => range(2,n+1).reduce((a,c) => a * c, 1)

/**
 * @template T
 * @param {T[][]} arrs 
 * @return {Generator<T[]>}
 * Example: f([[1,2],[3,4]]) -> [1,3] [1,4] [2,3] [2,4]
 */
export const multiSampleCombos = function*(arrs){
  const [first, ...rest] = arrs;
  if(!first) yield [];
  else for(const combo of multiSampleCombos(rest)){
    for(const elem of first){
      yield [elem, ...combo];
    }
  }
}

export const naturalNumGenerator = function*(){
  for(let i = 1; true; i++) yield i;
}

/**
 * @template T
 * @param {Iterator<T>} gen 
 * @param {number} amount
 * @returns {T[]}
 */
export const takeFromIterator = (gen, amount) => {
  const items = new Array(amount);
  for(let i = 0; i < amount; i++) items[i] = gen.next().value;
  return items;
}

/**
 * @template T
 * @param {Iterator<T>} itr 
 */
export const takeAll = itr => {
  const items = [];
  while(true){
    const value = itr.next();
    if(value.done) break;
    items.push(value.value);
  }
  return items;
}

/**
 * @template T
 * @template R
 * @param {() => Generator<T>} gen 
 * @param {(value: T) => R} fn
 * @returns {() => Generator<R>}
 */
export const mapGenerator = (genFn, fn) => {
  return function*(){
    const gen = genFn();
    while(true){
      const value = gen.next();
      if(!value.done) yield fn(value.value)
      else return fn(value.value);
    }
  }
}

/**
 * @template T
 * @template R
 * @param {() => Generator<T>} gen1 
 * @param {() => Generator<R>} gen2 
 * @returns {() => Generator<[T,R]>}
 */
export const zipGenerators = (genFn1, genFn2) => {
  return function*(){
    const gen1 = genFn1();
    const gen2 = genFn2();
    while(true){
      const value1 = gen1.next();
      const value2 = gen2.next();
      if(!value1.done && !value2.done) yield [value1.value, value2.value];
      else return [value1.value, value2.value];
    }
  }
}

/**
 * @template T
 * @param {() => Generator<T>} genFn 
 * @param {(value: T) => boolean} pred
 * @returns {() => Generator<T>}
 */
export const filterGenerator = (genFn, pred) => {
  return function*(){
    const gen = genFn();
    while(true){
      const value = gen.next();
      if(!pred(value.value)) continue;
      if(!value.done) yield value.value
      else return value.value;
    }
  }
}

/**
 * @template T
 * @param {() => Generator<T>} genFn 
 * @param {T[]} values
 * @returns {() => Generator<T>}
 */
export const prependConstants = (genFn, values) => {
  return function*(){
    for(const value of values) yield value;
    yield* genFn();
  }
}

/**
 * @template T
 * @param {T} x 
 * @returns T
 */
export const id = x => x;

/**
 * Must be sorted
 * NOTE 
 * @template T
 * @param {T[]} arr 
 * @param {T} target 
 * @param {number=} start 
 * @param {number=} end 
 * @param {(arg: T) => number=} accessor
 * @returns {T}
 */
export const binSearch = (arr, target, start, end, accessor) => {
  start = start ?? 0;
  end = end ?? arr.length;
  accessor = accessor ?? id;
  const accessedTarget = accessor(target);
  while(start <= end){
    let mid = start + Math.floor((end - start) / 2);
    const accessedMid = accessor(arr[mid]);
    if(accessedMid === accessedTarget) return mid;
    else if(accessedMid < accessedTarget) start = mid + 1;
    else end = mid - 1;
  }
  return start - 1;
}

/**
 * @param {number} n 
 * @param {number} lower 
 * @param {number} upper 
 * @param {boolean=} inclusive 
 */
export const inRange = (n, lower, upper, inclusive) => {
  if(inclusive) return lower <= n && n <= upper;
  return lower < n && n < upper;
}

/**
 * @template T
 * @param {T[]} arr 
 * @param {(arg: T) => boolean} pred 
 */
export const countMatches = (arr, pred) => arr.reduce((acc, current) => pred(current) ? acc + 1 : acc, 0);
