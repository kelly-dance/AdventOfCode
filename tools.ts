export const Digits = [0,1,2,3,4,5,6,7,8,9];

/**
 * [start, end)
 * if only 1 param is passed start is 0 and param is end
 * increment defaults to 1
 */
export function range(start: number, end: number, inc?: number): number[];
export function range(end: number): number[];
export function range(start: number, end?: number, inc: number = 1): number[] {
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

/**
 * Generates primes less than max
 */
export function sieve(max: number): number[] {
  let arr = bools(max);
  const count = arr.reduce((acc,el) => el ? acc + 1 : acc, 0); // bc having to make new resized array is slow
  const primes = new Array(count);
  for(let i = 0, j = 0; i < arr.length; i++) if(arr[i]) primes[j++] = i;
  return primes;
}

/**
 * Generates a boolean array where arr[i] = isPrime(i)
 */
export function bools(max: number): boolean[] {
  let arr = new Array(max).fill(true);
  arr[0] = arr[1] = false;
  for(let i = 2; i <= Math.sqrt(max); i++){
    if(arr[i]) for(let j = i * i; j < max; j += i) arr[j] = false;
  }
  return arr;
}

export function mod(n: number, t: number){
  if(n>=0) return n%t;
  else return t+(n%t);
}

export function sudokuSolve(b: number[][]): number[][] | undefined {
  function solve(b: number[][]){
    let x: number;
    let y = -1;
    for(x = 0; x < 9 && y == -1; x++){
      y = b[x].indexOf(0);
    } x--;
    if(x == 8 && y == -1) return b;
    else {
      let cs = sudokuChoices(b, x, y);
      cs.forEach(c=>{
        let nb = b.map(arr=>arr.slice());
        nb[x][y]=c;
        let sol = solve(nb);
        if(sol) return sol;
      });
      return undefined;
    }
  }
  return solve(b);
}

export function sudokuChoices(arr: number[][], x: number, y: number){
  let seen = new Set(Digits.slice(1));
  for(let i = 0; i < 9; i++){
    seen.delete(arr[x][i]);
    seen.delete(arr[i][y]);
    seen.delete(arr[Math.floor(i/3)+3*Math.floor(x/3)][(i%3)+3*Math.floor(y/3)]);
  }
  return [...seen];
}

export const sum = (nums: number[]) => nums.reduce((a, n) => a + n, 0);
export const sumBigInt = (nums: bigint[]) => nums.reduce((a, n) => a + n, 0n);

export const product = (nums: number[]) => nums.reduce((a, n) => a * n, 1);
export const productBigInt = (nums: bigint[]) => nums.reduce((a, n) => a * n, 1n);

/**
 * PLEASE PASS PRIMES TO THIS OR MEGA SLOW
 * @param {number} n 
 * @param {number[]} primes 
 * @returns {number[]}
 */
export const primeFactors = (n: number, given: number[] | null = null) => {
  const primes = given || sieve(n);
  let runningTotal = n;
  const factors: number[] = [];
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

export const countRepeats = <T>(arr: T[]): Map<T, number> => {
  const map = new Map();
  for(const elem of arr){
    if(!map.has(elem)) map.set(elem, 1);
    else map.set(elem, map.get(elem) + 1);
  }
  return map;
}

export const intDiv = (a: number, b: number) => Math.floor(a/Number(b));

export const digitsOf = (n: number, pad: number | null = null) => {
  if(n<0) throw new Error('Invalid argument, number must be positive');
  const digits: number[] = [];
  while(n > 0){
    digits.push(n % 10);
    n = intDiv(n, 10);
  }
  if(pad !== null) while(digits.length < pad) digits.push(0);
  return digits;
}

export const digitsOfBigInt = (n: bigint, pad: number | null = null) => {
  if(n < 0) throw new Error('Invalid argument, number must be positive');
  const digits: bigint[] = [];
  while(n > 0){
    digits.push(n % 10n);
    n /= 10n;
  }
  if(pad !== null) while(digits.length < pad) digits.push(0n);
  return digits;
}

export const isPalindrome = (n: number) => {
  const digits =digitsOf(n);
  for(let i = 0; i < digits.length / 2; i++) if(digits[i] !== digits[digits.length - i - 1]) return false;
  return true;
}

export const isPalindromeBigInt = (n: bigint) => {
  const digits = digitsOfBigInt(n);
  for(let i = 0; i < digits.length / 2; i++) if(digits[i] !== digits[digits.length - i - 1]) return false;
  return true;
}

export const numberFromDigits = (digits: number[]) => sum(digits.map((n, i) => n * 10 ** i));

export const bigIntFromDigits = (digits: bigint[]) => sumBigInt(digits.map((n, i) => n * 10n ** BigInt(i)));

export const reverseNumber = (n: number) => numberFromDigits(digitsOf(n).reverse());

export const reverseBigInt = (n: bigint) => bigIntFromDigits(digitsOfBigInt(n).reverse());

export const combos = function*<T>(arr: T[], times: number): Generator<T[]> {
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

export const combosWithLowerTimes = function*<T>(arr: T[], times: number): Generator<T[]>{
  for(let i = times; i > 0; i--){
    yield* combos(arr,i);
  }
}

export const memoize = <P, R>(fn: ((arg0: P) => R), defaults?: Map<P, R>): ((arg0: P) => R) => {
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

export const factors = (n: number) => {
  const factors = [1];
  for(let i = 2; i < Math.ceil(Math.sqrt(n)); i++){
    if(Number.isInteger(n/i)) factors.push(i, n/i);
  }
  return factors;
}

export const zip = <R extends any[][]>(
  ...arrs: R
): { [K in keyof R]: R[K] extends (infer T)[] ? T : R[K] }[] => 
  range(
    Math.min(...arrs.map(a => a.length))
  ).map(i => arrs.map(a => a[i])) as any;

export const readFile = (path: string) => (new TextDecoder("utf-8")).decode(Deno.readFileSync(path));

export const isCoprime = (a: number, b: number) => {
  for(let i = 2; i <= Math.min(a, b); i++) if(b % i === 0 && a % i === 0) return false;
  return true;
}

export const totient = (n: number) => {
  let counter = 0;
  for(const num of range(1,n)) {
    if(isCoprime(n, num)) counter++;
  }
  return counter;
}

export const newTotient = (n: number, given?: number[]) => {
  const primes = given || sieve(n);
  let t = n;
  for(const p of primes){
    if(n % p === 0) t *= 1 - 1/p;
  }
  if(t === n) t--;
  return Math.round(t);
}

export const totientsUpTo = (target: number) => {
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

export const debugDecorator = (label: string) => <T extends [], R>(fn: ((...args: T) => R)) => (...args: T) => {
  console.log(`called`, label, 'args', args)
  const result = fn(...args)
  console.log(`returned from`, label, result, 'args', args)
  return result;
}

export const factorial = (n: number) => range(2,n+1).reduce((a,c) => a * c, 1)

/**
 * Example: f([[1,2],[3,4]]) -> [1,3] [1,4] [2,3] [2,4]
 * needs better typing
 */
export const multiSampleCombos = function*<T extends any[]>(
  arrs: T[]
): Generator<T> {
  const [first, ...rest] = arrs;
  if(!first) yield [] as unknown as T;
  else for(const combo of multiSampleCombos(rest)){
    for(const elem of first){
      yield [elem, ...combo] as T;
    }
  }
}

export type FutureGen<T> = () => Generator<T>;

export const naturalNumGenerator = function*(){
  for(let i = 1; true; i++) yield i;
}

export const takeFromIterator = <T>(
  gen: Iterator<T>,
  amount: number
): T[] => {
  const items = new Array(amount);
  for(let i = 0; i < amount; i++) items[i] = gen.next().value;
  return items;
}

export const takeAll = <T>(itr: Iterator<T>) => {
  const items: T[] = [];
  while(true){
    const value = itr.next();
    if(value.done) break;
    items.push(value.value);
  }
  return items;
}

export const mapGenerator = <T, R>(
  genFn: (FutureGen<T>
), fn: ((value: T) => R)): FutureGen<R> => {
  return function*(){
    const gen = genFn();
    while(true){
      const value = gen.next();
      if(!value.done) yield fn(value.value)
      else return fn(value.value);
    }
  }
}

export const zipGenerators = <T extends (FutureGen<any>)[]>(
  ...genFns: T
): (FutureGen<{ [K in keyof T]: T[K] extends (FutureGen<infer S>) ? S : T[K] }>) => {
  return function*(){
    const gens = genFns.map(fn => fn());
    while(true){
      const values = gens.map(gen => gen.next());
      if(values.some(value => value.done)) return;
      yield values.map(value => value.value) as any;
    }
  }
}

export function* zipIterables<T extends Iterable<any>[]>(
  ...args: T
): Iterable<{ [K in keyof T]: T[K] extends Iterable<infer U> ? U : T[K] }> {
  const iterators = args.map((x) => x[Symbol.iterator]());
  while (true) {
      const next = iterators.map((i) => i.next());
      if (next.some((v) => v.done)) break;
      yield next.map((v) => v.value) as any;
  }
}

export const filterGenerator = <T>(genFn: FutureGen<T>, pred: ((value: T) => boolean)): FutureGen<T> => {
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

export const prependConstants = <T>(genFn: FutureGen<T>, values: T[]): FutureGen<T> => {
  return function*(){
    for(const value of values) yield value;
    yield* genFn();
  }
}

export const id = <T>(x: T) => x;

export const binSearch = <T>(
  arr: T[],
  target: T,
  accessor?: ((arg: T) => number),
  start?: number,
  end?: number,
): number => {
  let kStart = start ?? 0;
  let kEnd = end ?? arr.length;
  if(!accessor){
    if(typeof arr[0] === 'number'){
      accessor = accessor ?? id as ((arg: T) => number);
    } else {
      throw Error('binSearch on non number arrays requires and accessor');
    }
  }
  const accessedTarget = accessor(target);
  while(kStart <= kEnd){
    let mid = kStart + Math.floor((kEnd - kStart) / 2);
    const accessedMid = accessor(arr[mid]);
    if(accessedMid === accessedTarget) return mid;
    else if(accessedMid < accessedTarget) kStart = mid + 1;
    else kEnd = mid - 1;
  }
  return kStart - 1;
}

export const inRange = (n: number, lower: number, upper: number, inclusive: boolean) => {
  if(inclusive) return lower <= n && n <= upper;
  return lower < n && n < upper;
}

/**
 * @template T
 * @param {T[]} arr 
 * @param {(arg: T) => boolean} pred 
 */
export const countMatches = <T>(arr: T[], pred: ((arg: T) => boolean)) => arr.reduce((acc, current) => pred(current) ? acc + 1 : acc, 0);
