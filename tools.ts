import { Md5Hash } from 'https://deno.land/x/checksum@1.4.0/md5.ts';
import { encodeToString } from 'https://deno.land/std@0.53.0/encoding/hex.ts';

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
    { length: Math.ceil((end - start) / inc) },
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
  else return (t+(n%t))%t;
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

export const sumVec2 = (nums: Vec2[]) => nums.reduce((a, n) => {
  a.x += n.x;
  a.y += n.y;
  return a;
}, new Vec2(0));

export const sumVec3 = (nums: Vec3[]) => nums.reduce((a, n) => {
  a.x += n.x;
  a.y += n.y;
  a.z += n.z;
  return a;
}, new Vec3(0));

export const product = (nums: number[]) => nums.reduce((a, n) => a * n, 1);
export const productBigInt = (nums: bigint[]) => nums.reduce((a, n) => a * n, 1n);

/**
 * PLEASE PASS PRIMES TO THIS OR MEGA SLOW
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

export const groupBy = <T, K>(arr: T[], pred: (arg: T) => K): DefaultMap<K, T[]> => {
  const map = new DefaultMap<K, T[]>(() => []);
  for(const val of arr){
    const computed = pred(val);
    map.get(computed).push(val);
  }
  return map;
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
  const digits = digitsOf(n);
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
  function* helper(index: number, result: T[]): Generator<T[]> {
    if(index === times - 1){
      for(const elem of arr) {
        result[index] = elem;
        yield result;
      }
    }else{
      for(const elem of arr){
        result[index] = elem;
        yield* helper(index + 1, result);
      }
    }
  }
  yield* helper(0, new Array(times));
}

export const nonRepeatingCombos = function*<T>(arr: T[], times: number): Generator<T[]> {
  function* helper(index: number, start: number, result: T[]): Generator<T[]> {
    if(index === times - 1){
      for(let i = start; i < arr.length; i++){
        result[index] = arr[i];
        yield result;
      }
    }else{
      for(let i = start; i < arr.length; i++){
        const elem = arr[i];
        result[index] = elem;
        yield* helper(index + 1, i + 1, result);
      }
    }
  }
  yield* helper(0, 0, new Array(times));
}

export const combosWithLowerTimes = function*<T>(arr: T[], times: number): Generator<T[]>{
  for(let i = times; i > 0; i--){
    yield* combos(arr,i);
  }
}

export const nonRepeatingCombosWithLowerTimes = function*<T>(arr: T[], times: number): Generator<T[]>{
  for(let i = times; i > 0; i--){
    yield* nonRepeatingCombos(arr,i);
  }
}

export const memoize = <P, R>(fn: ((arg0: P) => R), defaults?: Map<P, R>): ((arg0: P) => R) => {
  const cache = defaults || new Map<P,R>();
  return arg => {
    if(cache.has(arg)) return cache.get(arg) as R;
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
): { [K in keyof R]: R[K] extends (infer T)[] ? T : never }[] => 
  range(
    Math.min(...arrs.map(a => a.length))
  ).map(i => arrs.map(a => a[i])) as any;

export const zipWith = <F, R extends any[][]>(
  mapper: (args: { [K in keyof R]: R[K] extends (infer T)[] ? T : R[K] }) => F,
  ...arrs: R
): F[] => zip(...arrs).map(group => mapper(group as any));

export const readFile = (path: string) => (new TextDecoder("utf-8")).decode(Deno.readFileSync(path)).replaceAll('\r','');

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

/**
 * totientsUpTo(n)[i <= n] = phi(i)
 */
export const totientsUpTo = (target: number) => {
  const phi = range(0,target+1);
  for(let n = 2; n <= target; n++){
      if(phi[n] == n){ // this number is prime
        phi[n]--; // phi(prime) = prime - 1
        for(let i = n * 2; i <= target; i += n) // loop through multiples of this number
          phi[i] *= 1 - (1 / n); //removes the stuff
      }
  }
  return phi;
}

/**
 * outputs totients starting from phi(i), phi(2) ... phi(target)
 */
export const totientsUpToGenerator = function*(target: number){
  const phi = range(0,target+1);
  yield 1;
  for(let n = 2; n <= target; n++){
      if(phi[n] == n){ // this number is prime
        phi[n]--; // phi(prime) = prime - 1
        for(let i = n * 2; i <= target; i += n) // loop through multiples of this number
          phi[i] *= 1 - (1 / n); //removes the stuff
      }
      yield phi[n];
  }
  return phi;
}

export const debugDecorator = (label: string) => <T extends any[], R>(fn: ((...args: T) => R)) => (...args: T) => {
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

export function* mapGenerator<T, R>(gen: Generator<T>, fn: ((value: T) => R)): Generator<R> {
  while(true){
    const value = gen.next();
    if(value.done) return;
    yield fn(value.value)
  }
}

export function* zipGenerators<T extends Generator<any>[]>(
  ...gens: T
): (Generator<{ [K in keyof T]: T[K] extends (Generator<infer S>) ? S : T[K] }>){
  return function*(){
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

export function* filterGenerator<T>(gen: Generator<T>, pred: ((value: T) => boolean)): Generator<T> {
  while(true){
    const value = gen.next();
    if(value.done) return;
    if(!pred(value.value)) continue;
    yield value.value
  }
}

export function* prependConstants<T>(gen: Generator<T>, values: T[]): Generator<T> {
    for(const value of values) yield value;
    yield* gen;
}

export const id = <T>(x: T) => x;

export function succ(x: number): number;
export function succ(x: bigint): bigint;
export function succ(x: number | bigint) {
  if(typeof x === 'number') return x + 1;
  if(typeof x === 'bigint') return x + 1n;
  throw new Error('Successor is not defined for this type');
};

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

/**
 * checks in number is in (lower, upper) or if the inclusive parameter is sent then [lower, upper]
 */
export const inRange = (n: number, lower: number, upper: number, inclusive: boolean = false) => {
  if(inclusive) return lower <= n && n <= upper;
  return lower < n && n < upper;
}

/**
 * [lower, upper)
 */
export const inRangeStd = (n: number, lower: number, upper: number) => lower <= n && n < upper;

export const countMatches = <T>(arr: T[], pred: ((arg: T, index: number) => boolean)) => arr.reduce((acc, current, index) => pred(current, index) ? acc + 1 : acc, 0);

export const union = <T>(...sets: Set<T>[]): Set<T> => new Set(sets.map(s => [...s]).flat(1));

export const intersection = <T>(a: Set<T>, ...rest: Set<T>[]) => new Set<T>(takeAll(a.values()).filter(v => rest.every(set => set.has(v))));

export const subtractSets = <T>(a: Set<T>, b: Set<T>, destructive: boolean = false): Set<T> => {
  const base = destructive ? a : new Set(a);
  for(const val of b) base.delete(val);
  return base;
}

/** is `a` a subset of `b` */
export const isSubset = (a: Set<any>, b: Set<any>): boolean => {
  for(const elem of a) if(!b.has(elem)) return false;
  return true;
}

export const pickIntsFromString = (str: string) => str.match(/-?\d+/g)?.map(s => parseInt(s)) ?? [];

export class MultiMap<Ks extends any[], T> implements Map<Ks, T>{
  numKeys: number;
  root: Map<any, any>;
  size: number;

  constructor(numKeys: number){
    this.numKeys = numKeys;
    this.root = new Map();
    this.size = 0;
  }

  get(keys: Ks): T | undefined {
    if(keys.length !== this.numKeys) return;
    let current = this.root;
    for(let layer = 0; layer < this.numKeys; layer++){
      if(!current.has(keys[layer])) return undefined;
      current = current.get(keys[layer]);
    }
    return current as any as T;
  }

  set(keys: Ks, value: T) {
    if(keys.length !== this.numKeys) return this;
    let current = this.root;
    for(let layer = 0; layer < this.numKeys - 1; layer++){
      if(!current.has(keys[layer])) current.set(keys[layer], new Map())
      current = current.get(keys[layer]);
    }
    if(!current.has(keys[this.numKeys - 1])) this.size++;
    current.set(keys[this.numKeys - 1], value);
    return this;
  }

  has(keys: Ks){
    if(keys.length !== this.numKeys) return false;
    let current = this.root;
    for(let layer = 0; layer < this.numKeys - 1; layer++){
      if(!current.has(keys[layer])) return false;
      current = current.get(keys[layer]);
    }
    return current.has(keys[this.numKeys - 1]);
  }

  *entries(): Generator<[Ks, T]> {
    const self = this;
    function* yieldFrom(prepend: any, map: Map<any, any>, layer: number): Generator<[any, T]> {
      if(layer === self.numKeys) {
        for(const [key, entry] of map.entries()){
          yield [[...prepend, key], entry]
        }
      } else {
        for(const [key, entry] of map.entries()){
          yield* yieldFrom([...prepend, key], entry, layer + 1);
        }
      }
    }
    yield* yieldFrom([], this.root, 1);
  }

  *keys(): Generator<Ks> {
    for(const [keys, _] of this.entries()) yield keys;
  }

  *values(): Generator<T> {
    for(const [_, value] of this.entries()) yield value;
  }

  clear(){
    this.root = new Map();
    this.size = 0;
  }

  delete(keys: Ks) {
    if(keys.length !== this.numKeys) return false;
    let current = this.root;
    for(let layer = 0; layer < this.numKeys - 1; layer++){
      if(!current.has(keys[layer])) return false;
      current = current.get(keys[layer]);
    }
    const didDelete = current.delete(keys[this.numKeys - 1]);
    if(didDelete) this.size--;
    return didDelete;
  }

  forEach(cb: (value: T, keys: Ks, map: this) => void) {
    for(const [keys, value] of this.entries()){
      cb(value, keys, this);
    }
  }

  get [Symbol.toStringTag](){
    return 'MultiMap';
  }

  *[Symbol.iterator](){
    yield* this.entries();
  }

  /** does nothing if key does not exist */
  apply(key: Ks, fn: (val: T, key: Ks) => T){
    if(this.has(key)) this.set(key, fn(this.get(key) as T, key));
    return this;
  }
}

export class DefaultMultiMap<K extends any[], V> extends MultiMap<K, V>{
  derive: (key: K) => V;

  constructor(keys: Sizes, deriveDefault: (key: K) => V){
    super(keys);
    this.derive = deriveDefault;
  }

  get(key: K): V {
    const stored = super.get(key);
    if(stored === undefined) return this.derive(key);
    return stored;
  }

  apply(key: K, fn: (val: V, key: K) => V): this {
    this.set(key, fn(this.get(key), key));
    return this;
  }

  has(key: K){
    return true;
  }
}

export const memoizeMulti = <Ks extends any[], R>(fn: (...args: Ks) => R, defaults?: MultiMap<Ks, R>): ((...args: Ks) => R) => {
  const cache = defaults || new MultiMap<Ks, R>(fn.length);
  return (...args: Ks) => {
    if(cache.has(args)) return cache.get(args)!;
    const result = fn(...args);
    cache.set(args, result);
    return result;
  }
}

export const enumerate = <T>(arr: T[]): [value: T, index: number][] => arr.map((v, i) => [v, i]);

export class DefaultMap<K, V> extends Map<K, V>{
  constructor(private derive: (key: K) => V){
    super();
  }

  get(key: K): V {
    const stored = super.get(key);
    if(stored === undefined) {
      const computed = this.derive(key);
      this.set(key, computed)
      return computed;
    }
    return stored;
  }

  apply(key: K, fn: (val: V, key: K) => V): this {
    this.set(key, fn(this.get(key), key));
    return this;
  }

  has(key: K){
    return true;
  }
}

export const takeFirst = <T>(iter: Iterator<T>) => iter.next().value;

export  function* subSequences<T>(arr: T[], minLength = 1, maxLength = Infinity): Generator<T[]> {
  if(minLength === 0){
    yield [];
    minLength = 1;
  }
  for(let offset = 0; offset <= arr.length - minLength; offset++){
    for(let length = minLength; length <= Math.min(arr.length - offset, maxLength); length++){
      yield arr.slice(offset, offset + length);
    }
  }
}

export function* subSequencesOfSize <T>(arr: T[], size: number): Generator<T[]> {
  for(let offset = 0; offset < arr.length - size; offset++){
    yield arr.slice(offset, offset + size);
  }
}

export const iteratorSome = <T>(iter: Iterator<T>, pred: (arg: T) => boolean) => {
  while(true){
    const { done, value } = iter.next();
    if(done) return false;
    if(pred(value)) return true;
  }
}

export const concat = <T>(arrs: T[][]): T[] => arrs.reduce((acc, cur) => [...acc, ...cur], []);

export const deepCopyArray = <T extends any[]>(arr: T): T => arr.map(elem => Array.isArray(elem) ? deepCopyArray(elem) : elem) as T;

export type Sizes = 1 | 2 | 3 | 4 | 5;
export type MultiDimArray<D extends Sizes, T> = [T, T[], T[][], T[][][], T[][][][], T[][][][][]][D];
export type TupleSizes<D extends Sizes, T> = [[], [T], [T,T], [T,T,T], [T,T,T,T], [T,T,T,T,T]][D];

export class Grid<D extends Sizes, T>{
  private internal: MultiDimArray<D, T>;
  dimensions: D;
  sizes: TupleSizes<D, number>;
  OUT_OF_BOUNDS = {};

  constructor(dimensions: D, data: MultiDimArray<D, T>, skipCopy: boolean = false){
    this.internal = skipCopy ? data : deepCopyArray(data);
    this.dimensions = dimensions;
    const sizes: number[] = [];
    let prev: any = data;
    for(let d = 0; d < dimensions; d++) {
      sizes.push(prev.length)
      prev = prev[0];
    }
    this.sizes = sizes as TupleSizes<D, number>;
  }

  *neighborOffsets(): Generator<TupleSizes<D, number>> {
    yield* combos([-1,0,1], this.dimensions) as Generator<TupleSizes<D, number>>;
  }

  private getHelper(arr: any, coords: number[], idx: number = 0, wrapAround: boolean = true): T[] | T | Grid<D,T>["OUT_OF_BOUNDS"] {
    if(arr === undefined) return this.OUT_OF_BOUNDS;
    if(coords.length === idx) return arr;
    return wrapAround ? 
      this.getHelper(arr[mod(coords[idx], arr.length)], coords, idx + 1, wrapAround) :
      this.getHelper(arr[coords[idx]], coords, idx + 1, wrapAround);
  }

  get(coords: TupleSizes<D, number>, wrapAround: boolean = false): T | Grid<D,T>["OUT_OF_BOUNDS"] {
    return this.getHelper(this.internal, coords, 0, wrapAround);
  }

  set(coords: TupleSizes<D, number>, value: T): this {
    const arr = this.getHelper(this.internal, coords.slice(0, -1)) as T[];
    if(arr === this.OUT_OF_BOUNDS) return this;
    arr[mod(coords[coords.length - 1], arr.length)] = value;
    return this;
  }

  *castRay(
    origin: TupleSizes<D, number>,
    velocity: TupleSizes<D, number>,
    wrapAround: boolean = false
  ): Generator<[coords: TupleSizes<D, number>, data: T]> {
    for(let d = 1; true; d++){
      let current: TupleSizes<D, number> = zipWith(sum, origin as number[], velocity.map(n => n * d)) as TupleSizes<D, number>;
      if(!wrapAround && current.some((n, d) => inRangeStd(n, 0, this.sizes[d]))) return;
      yield [current, this.get(current, true) as any];
    }
  }

  neighbors(coords: TupleSizes<D, number>, wrapAround: boolean = false): [coords: TupleSizes<D, number>, data: T][]{
    const neighbors: [coords: TupleSizes<D, number>, data: T][] = [];
    for(const offset of this.neighborOffsets()){
      const specific = zipWith(sum, coords as number[], offset as number[]) as TupleSizes<D, number>;
      if(zipWith(([a,b]) => a === b, specific, coords).every(id)) continue;
      const data = this.get(specific, wrapAround);
      if(data !== this.OUT_OF_BOUNDS) neighbors.push([specific, data as any]);
    }
    return neighbors;
  }

  clone(): Grid<D, T> {
    return new Grid(this.dimensions, this.internal);
  }

  *entries(lazy = true): Generator<[coords: TupleSizes<D, number>, data: T]> {
    function* helper(arr: any[], coords: number[], dim: number): Generator<any> {
      for(let i = 0; i < arr.length; i++){
        const elem = arr[i];
        coords[dim] = i;
        if(Array.isArray(elem)) yield* helper(elem, coords, dim + 1);
        else yield [lazy ? coords : coords.slice(0), elem];
      }
    }
    yield* helper(this.internal, new Array(this.dimensions), 0);
  }

  keys(lazy = true): Generator<TupleSizes<D, number>> {
    return mapGenerator(this.entries(lazy), ([coords, _]) => coords);
  }

  *values(): Generator<T> {
    function* helper(arr: any[]): Generator<any> {
      for(let i = 0; i < arr.length; i++){
        const elem = arr[i];
        if(Array.isArray(elem)) yield* helper(elem);
        else yield elem;
      }
    }
    yield* helper(this.internal);
  }

  forEach(callback: (value: T, indicies: TupleSizes<D, number>, self: this) => void, lazy = true): this {
    for(const [index, value] of this.entries(lazy)){
      callback(value, index, this);
    }
    return this;
  }

  map<R>(callback: (value: T, indicies: TupleSizes<D, number>, self: this) => R, lazy = true): Grid<D, R> {
    const mapper = (arr: any[], coords: number[], dim: number): any => arr.map((elem, index) => {
      coords[dim] = index;
      if(Array.isArray(elem)) return mapper(elem, coords, dim + 1);
      return callback(elem as T, (lazy ? coords : coords.slice(0)) as TupleSizes<D, number>, this)
    })
    return new Grid<D, R>(this.dimensions, mapper(this.internal, new Array(this.dimensions), 0), true);
  }

  reduce<R>(reducer: (acc: R, value: T, indicies: TupleSizes<D, number>, self: this) => R, initial: R, lazy = true){
    let acc = initial;
    for(const [coords, value] of this.entries(lazy)){
      acc = reducer(acc, value, coords, this);
    }
    return acc;
  }

  asArray(copy: boolean = false): MultiDimArray<D, T> {
    return copy ? deepCopyArray(this.internal) : this.internal;
  }

  equals(other: Grid<Sizes, any>){
    if(this.dimensions !== other.dimensions) return false;
    for(const [a, b] of zipGenerators(this.values(), other.values())){
      if(a !== b) return false;
    }
    return true;
  }
}

export const first = <T>(arg: readonly [T, ...any]): T => arg[0];
export const second = <T>(arg: readonly [any, T, ...any]): T => arg[1];
export const third = <T>(arg: readonly [any, any, T, ...any]): T => arg[2];

export function add(x: number): (y: number) => number;
export function add(x: bigint): (y: bigint) => bigint;
export function add(x: string): (y: string | number | bigint) => string;
export function add(x: Vec2): (y: Vec2) => Vec2;
export function add(x: Vec3): (y: Vec3) => Vec3;
export function add<T>(x: Set<T>): (y: Set<T>) => Set<T>;
export function add<T>(x: number | bigint | string | Vec2 | Vec3 | Set<T>) {
  if(typeof x === 'number') return (y: number) => x + y;
  if(typeof x === 'bigint') return (y: bigint) => x + y;
  if(typeof x === 'string') return (y: string | number | bigint) => x + y;
  if(x instanceof Vec2) return (y: Vec2) => x.add(y);
  if(x instanceof Vec3) return (y: Vec3) => x.add(y);
  if(x instanceof Set) return (y: Set<T>) => union(x, y);
  throw new Error('add is not defined for this type');
}

export type Vec2Str = ReturnType<Vec2['toString']>
export class Vec2 {
  x: number;
  y: number;

  static ORIGIN = new Vec2(0);

  static SIDES = [
    new Vec2(0, 1),
    new Vec2(1, 0),
    new Vec2(0, -1),
    new Vec2(-1, 0),
  ]

  static DIAGONALS = [
    new Vec2(1, 1),
    new Vec2(1, -1),
    new Vec2(-1, 1),
    new Vec2(-1, -1),
  ]

  constructor(x: number, y?: number){
    this.x = x;
    this.y = y ?? x;
  }

  add(otr: Vec2): Vec2 {
    return new Vec2(this.x + otr.x, this.y + otr.y)
  }

  mult(scalar: number): Vec2 {
    return new Vec2(this.x * scalar, this.y * scalar)
  }

  sub(otr: Vec2): Vec2 {
    return this.add(otr.mult(-1));
  }

  div(scalar: number): Vec2 {
    return this.mult(1 / scalar);
  }

  len(){
    return (this.x ** 2 + this.y ** 2) ** 0.5;
  }

  manhattenLen(){
    return Math.abs(this.x) + Math.abs(this.y)
  }

  minDist(otr: Vec2){
    return Math.min(Math.abs(this.x - otr.x), Math.abs(this.y - otr.y))
  }

  eq(otr: Vec2, EPSILON = 0.00001){
    return Math.abs(this.x - otr.x) < EPSILON && Math.abs(this.y - otr.y) < EPSILON;
  }

  normalized(): Vec2 {
    return this.div(this.len())
  }

  angle(): number {
    return Math.atan2(this.y, this.x);
  }

  toString(): `${number},${number}` {
    return `${this.x},${this.y}`;
  }

  static read(s: string): Vec2 {
    return new Vec2(...s.split(',').map(s => Number(s)) as [number, number]);
  }
  
  get<T>(source: T[][]): T | undefined {
    return source[this.x]?.[this.y];
  }
}

export type Vec3Str = ReturnType<Vec3['toString']>
export class Vec3{
  x: number;
  y: number;
  z: number;

  static ORIGIN = new Vec3(0);

  constructor(x: number, y?: number, z?: number){
    this.x = x;
    this.y = y ?? x;
    this.z = z ?? x;
  }

  add(otr: Vec3): Vec3 {
    return new Vec3(this.x + otr.x, this.y + otr.y, this.z + otr.z)
  }

  mult(scalar: number): Vec3 {
    return new Vec3(this.x * scalar, this.y * scalar, this.z * scalar)
  }

  sub(otr: Vec3): Vec3 {
    return this.add(otr.mult(-1));
  }

  div(scalar: number): Vec3 {
    return this.mult(1 / scalar);
  }

  len(){
    return (this.x ** 2 + this.y ** 2 + this.z ** 2) ** 0.5;
  }

  manhattenLen(){
    return Math.abs(this.x) + Math.abs(this.y) + Math.abs(this.z)
  }

  minDist(otr: Vec3){
    return Math.min(Math.abs(this.x - otr.x), Math.abs(this.y - otr.y), Math.abs(this.z - otr.z))
  }

  eq(otr: Vec3, EPSILON = 0.00001){
    return Math.abs(this.x - otr.x) < EPSILON && Math.abs(this.y - otr.y) < EPSILON && Math.abs(this.z - otr.z) < EPSILON;
  }

  normalized(): Vec3 {
    return this.div(this.len())
  }

  toString(): `${number},${number},${number}` {
    return `${this.x},${this.y},${this.z}`;
  }

  static read(s: string): Vec3 {
    return new Vec3(...s.split(',').map(s => Number(s)) as [number, number, number]);
  }

  get<T>(source: T[][][]): T | undefined {
    return source[this.x]?.[this.y]?.[this.z];
  }
}

export function group<T>(arr: T[], size: 2): [T, T][]
export function group<T>(arr: T[], size: 3): [T, T, T][]
export function group<T>(arr: T[], size: 4): [T, T, T, T][]
export function group<T>(arr: T[], size: 5): [T, T, T, T, T][]
export function group<T>(arr: T[], size: number): T[][]
export function group<T>(arr: T[], size: number): T[][] {
  const result: T[][] = [];
  for(let i = 0; i <= arr.length - size; i += size) result.push(arr.slice(i, i + size));
  return result;
}

export const uint8ToHex = (()=>{
  const hexTable = new TextEncoder().encode('0123456789abcdef');
  const decoder = new TextDecoder();
  return (data: Uint8Array) => {
    const out = new Uint8Array(data.length * 2);
    for(let i = 0; i < data.length; i++){
      out[i * 2] = hexTable[data[i] >> 4]
      out[i * 2 + 1] = hexTable[data[i] & 0x0F]
    }
    return decoder.decode(out);
  }
})();

export const MD5 = (()=>{
  const md5 = new Md5Hash();
  const encoder = new TextEncoder();
  const that = {
    rawToRaw: (data: Uint8Array): Uint8Array => {
      return md5.digest(data);
    },
    rawToStr: (data: Uint8Array): string => {
      return encodeToString(that.rawToRaw(data));
    },
    strToRaw: (str: string): Uint8Array => {
      return that.rawToRaw(encoder.encode(str));
    },
    strToStr: (str: string): string => {
      return encodeToString(that.rawToRaw(encoder.encode(str)));
    },
  };
  return that;
})();

export function* permutations<T>(arr: T[]): Generator<T[]> {
  if(arr.length === 0) {
    yield [];
    return;
  }
  if(arr.length === 1) {
    yield arr.slice();
    return;
  }
  for(let i = 0; i < arr.length; i++){
    const without = [...arr.slice(0, i), ...arr.slice(i + 1)];
    for(const perm of permutations(without)){
      yield [arr[i], ...perm];
    }
  }
}

export const minBy = <T>(data: Iterable<T>, fn: (data: T, i: number) => number | bigint): T => {
  const gen = data[Symbol.iterator]();
  let cur = gen.next();
  if(cur.done) throw new Error('Cannot get max of empty iterable');
  let best: [number | bigint, T] = [Infinity, undefined as any];
  let i = 0;
  while(!cur.done){
    const score = fn(cur.value, i);
    if(score < best[0]) best = [score, cur.value];
    cur = gen.next();
    i++;
  }
  return best[1];
}

export const maxBy = <T>(data: Iterable<T>, fn: (data: T, i: number) => number | bigint): T => {
  const gen = data[Symbol.iterator]();
  let cur = gen.next();
  if(cur.done) throw new Error('Cannot get max of empty iterable');
  let best: [number | bigint, T] = [-Infinity, undefined as any];
  let i = 0;
  while(!cur.done){
    const score = fn(cur.value, i);
    if(score > best[0]) best = [score, cur.value];
    cur = gen.next();
    i++;
  }
  return best[1];
}

export const pairs = <R extends any[][]>(
  ...arrs: R
): { [K in keyof R]: R[K] extends (infer T)[] ? T : never }[] => {
  if(arrs.length === 0) return [];
  if(arrs.length === 1) return arrs[0].map(e => [e]) as any;
  const subPairs = pairs(...arrs.slice(1));
  return arrs[0].map(elem => subPairs.map(sub => [elem, ...sub])).flat(1) as any;
}

export const lcm = (x: number, y: number) => {
  return (x * y) / gcd(x, y);
}
 
export const gcd = (x: number, y: number) => {
  while(y) [x, y] = [y, x % y];
  return x;
}

export const mapFrom = <K, V>(pairs: [K, V][]): Map<K, V> => {
  const map = new Map<K, V>();
  for(const [k, v] of pairs) map.set(k, v);
  return map;
}

export const invertMap = <K, V>(map: Map<K, V>): Map<V, K[]> => {
  const inverted = new Map<V, K[]>();
  for(const [key, value] of map.entries()) {
    if(!inverted.has(value)) inverted.set(value, [key]);
    else inverted.get(value)!.push(key);
  }
  return inverted;
}

export const mapMap = <K, V, L, N>(map: Map<K, V>, fn: (key: K, val: V) => [L, N]): Map<L, N> => {
  const newMap = new Map<L, N>();
  for(const [key, val] of map.entries()){
    const [newKey, newVal] = fn(key, val);
    newMap.set(newKey, newVal);
  }
  return newMap;
}

const warningText = `Please don't repeatedly request this endpoint before it unlocks! The calendar countdown is synchronized with the server time; the link will be enabled on the calendar the instant this puzzle becomes available.`;
export const readAdvent = async (): Promise<string> => {
  const match = Deno.mainModule.match(/.*(\\|\/)(20\d\d)(\\|\/)(\d\d).(js|ts)/);
  if(!match) throw new Error('Invalid script location or name. Cannot figure out the currnt year / day.')
  const year = match[2];
  const day = match[4];
  const exists = [...Deno.readDirSync(`./inputs`)].some(f => f.name === `${day}.txt`);
  if(exists) {
    const text = readFile(`./inputs/${day}.txt`);
    if(!text.includes(warningText) && text.length) return text;
  }
  const req = await fetch(
    `https://adventofcode.com/${year}/day/${parseInt(day)}/input`,
    { headers: { Cookie: `session=${readFile('../session')}` } }
  );
  let text = await req.text();
  if(text.endsWith('\n')) text = text.substring(0, text.length - 1);
  Deno.writeTextFileSync(`./inputs/${day}.txt`, text);
  return text;
}

export const printChar = (()=>{
  let acc = '';
  return (char: string) => {
    if(char.length > 1) throw new Error('Use this only with single char strings (or 0)');
    if(char === '\n') {
      console.log(acc);
      acc = '';
    } else acc += char;
  }
})();

export const transpose = <T>(arr: T[][]): T[][] => {
  return range(arr[0].length).map(j => range(arr.length).map(i => arr[i][j]));
}

export const irange = (start: number, end: number, step: number = 1): number[] => {
  const size = Math.abs(end - start) / step;
  if(size % 1 !== 0) throw new Error('Must be a whole number of steps from start to end!');
  const dir = Math.sign(end - start);
  return range(size + 1).map(i => start + step * dir * i);
}

export class OMap<K, V> implements Map<K, V> {
  public internalMap: Map<string, [K, V]>;
  [Symbol.toStringTag]: string;

  constructor(public resolver: (k: K) => string){
    this.internalMap = new Map();
  }


  clear(): void {
    this.internalMap.clear();
  }

  delete(key: K): boolean {
    return this.internalMap.delete(this.resolver(key));
  }

  *entries(): IterableIterator<[K, V]> {
    for(const [resolved, pair] of this.internalMap.entries()){
      yield pair;
    }
  }

  forEach(cb: (val: V, key: K, map: Map<K, V>) => any, thisArg?: any): void {
    for(const [res, [key, val]] of this.internalMap.entries()){
      cb.call(thisArg, val, key, this as any as Map<K, V>); // loooool
    }
  }

  get(key: K): V | undefined {
    return this.internalMap.get(this.resolver(key))?.[1];
  }

  has(key: K): boolean {
    return this.internalMap.has(this.resolver(key));
  }

  *keys(): IterableIterator<K> {
    for(const [res, [key, val]] of this.internalMap.entries()){
      yield key;
    }
  }

  set(key: K, val: V): this {
    this.internalMap.set(this.resolver(key), [key, val]);
    return this;
  }

  get size(){
    return this.internalMap.size;
  }

  *values(): IterableIterator<V> {
    for(const [res, [key, val]] of this.internalMap.entries()){
      yield val;
    }
  }

  [Symbol.iterator](): IterableIterator<[K, V]> {
    return this.entries();
  }

  apply(key: K, fn: (val: V | undefined, key: K) => V): this {
    this.set(key, fn(this.get(key), key));
    return this;
  }
}

export class DefaultOMap<K, V> extends OMap<K, V>{
  constructor(resolver: (k: K) => string, private derive: (key: K) => V){
    super(resolver);
  }

  get(key: K): V {
    const stored = super.get(key);
    if(stored === undefined) {
      const computed = this.derive(key);
      this.set(key, computed)
      return computed;
    }
    return stored;
  }

  apply(key: K, fn: (val: V, key: K) => V): this {
    this.set(key, fn(this.get(key), key));
    return this;
  }

  has(key: K){
    return true;
  }
}

export class OSet<T> implements Set<T> {
  public internalMap: Map<any, T>;
  [Symbol.toStringTag]: string;

  constructor(public resolver: (val: T) => string){
    this.internalMap = new Map();
  }

  clear(): void {
    this.internalMap.clear();
  }

  delete(key: T): boolean {
    return this.internalMap.delete(this.resolver(key));
  }

  *entries(): IterableIterator<[T, T]> {
    for(const [resolved, value] of this.internalMap.entries()){
      yield [value, value];
    }
  }

  forEach(cb: (val: T, key: T, set: Set<any>) => any, thisArg?: any): void {
    for(const [res, value] of this.internalMap.entries()){
      cb.call(thisArg, value, value, this as any as Set<T>); // loooool
    }
  }

  has(val: T): boolean {
    return this.internalMap.has(this.resolver(val));
  }

  add(val: T): this {
    this.internalMap.set(this.resolver(val), val);
    return this;
  }

  *keys(): IterableIterator<T> {
    for(const [res, val] of this.internalMap.entries()){
      yield val;
    }
  }

  *values(): IterableIterator<T> {
    yield* this.keys();
  }

  get size(){
    return this.internalMap.size;
  }

  [Symbol.iterator](): IterableIterator<T> {
    return this.values();
  }
}

export function* multiLoop<S extends Sizes, T extends any>(depth: S, arr: MultiDimArray<S, T>): Generator<[T, ...TupleSizes<S, number>]> {
  if(depth === 1){
    for(let i = 0; i < arr.length; i++)
      yield [arr[i], i] as any;
  } else {
    for(let i = 0; i < arr.length; i++){
      for(const sub of multiLoop(depth - 1 as Sizes, arr[i] as any)){
        yield [...sub, i] as any;
      }
    }
  }
}

export type Node<T> = {
  value: T,
  left?: Node<T>,
  right?: Node<T>,
  parent?: Node<T>,
}
export class SortedSet<T>{
  public root: Node<T> | undefined;

  constructor(public comparator: (a: T, b: T) => number) {}

  public add(value: T){
    if(this.root === undefined) {
      this.root = { value }
      return;
    }
    const helper = (node: Node<T>) => {
      const comp = this.comparator(value, node.value);
      if(comp === 0) return;
      if(comp < 0){
        if(node.left === undefined) node.left = { value, parent: node };
        else helper(node.left);
      }else{
        if(node.right === undefined) node.right = { value, parent: node };
        else helper(node.right);
      }
    }
    helper(this.root);
  }

  public remove(value: T){
    if(this.root === undefined) return;
    const helper = (node?: Node<T>) => {
      if(!node) return;
      const comp = this.comparator(value, node.value);
      if(comp === 0){
        if(!node.left && !node.right) { // no children
          this.setParentsChild(node, undefined)
        } else if(!!node.left !== !!node.right) { // 1 child
          if(node.left) this.setParentsChild(node, node.left);
          else this.setParentsChild(node, node.right);
        } else { // both children
          let preorder = node.left!;
          while(preorder.left || preorder.right) preorder = preorder.right ?? preorder.left!;
          this.setParentsChild(preorder, undefined);
          node.value = preorder.value;
        }
      }else if(comp < 0) helper(node.left);
      else helper(node.right);
    }
    helper(this.root);
  }

  public reduce<R>(reducer: (acc: R, val: T) => R, init: R): R {
    if(this.root === undefined) return init;
    const reduceWith = (acc: R, node: Node<T>): R => {
      if(node.left) acc = reduceWith(acc, node.left);
      acc = reducer(acc, node.value);
      if(node.right) acc = reduceWith(acc, node.right);
      return acc;
    }
    return reduceWith(init, this.root);
  }

  public has(value: T){
    const helper = (node?: Node<T>): boolean => {
      if(!node) return false;
      const comp = this.comparator(value, node.value);
      if(comp === 0) return true;
      if(comp < 0) return helper(node.left);
      else return helper(node.right);
    }
    return helper(this.root);
  }

  private setParentsChild(node: Node<T>, to?: Node<T>){
    if(node === this.root) this.root = to;
    else if(node.parent!.left === node) node.parent!.left = to;
    else node.parent!.right = to;
  }
}

export class Heap<T>{
  private items: T[];

  constructor(private comparator: (a: T, b: T) => number){
    this.items = [];
  }

  get size(){ return this.items.length }

  private getParentIndex(i: number) { return Math.floor((i - 1) / 2) }
  private getLeftIndex(i: number) { return i * 2 + 1 }
  private getRightIndex(i: number) { return i * 2 + 2 }

  private hasParent(i: number) { return this.getParentIndex(i) < this.size && this.getParentIndex(i) >= 0 }
  private hasLeft(i: number) { return this.getLeftIndex(i) < this.size && this.getLeftIndex(i) >= 0 }
  private hasRight(i: number) { return this.getRightIndex(i) < this.size && this.getRightIndex(i) >= 0 }

  private parent(i: number) { return this.items[this.getParentIndex(i)] }
  private left(i: number) { return this.items[this.getLeftIndex(i)] }
  private right(i: number) { return this.items[this.getRightIndex(i)] }

  private swap(a: number, b: number){
    [this.items[a], this.items[b]] = [this.items[b], this.items[a]];
  }

  public top(): T {
    if(!this.size) throw new Error('Cannot get top element of empty heap');
    return this.items[0];
  }

  public pop(): T {
    if(!this.size) throw new Error('Cannot pop element from empty heap');
    if(this.size === 1) return this.items.pop()!;
    const item = this.items[0];
    this.items[0] = this.items.pop()!;
    this.heapifyDown();
    return item;
  }

  public add(item: T) {
    this.items.push(item);
    this.heapifyUp();
  }

  public heapifyUp(){
    let i = this.items.length - 1;
    while(this.hasParent(i) && this.comparator(this.parent(i), this.items[i]) > 0){
      this.swap(this.getParentIndex(i), i);
      i = this.getParentIndex(i);
    }
  }

  public heapifyDown(){
    let i = 0;
    while(this.hasLeft(i)){
      let smaller = this.getLeftIndex(i);
      if(this.hasRight(i) && this.comparator(this.right(i), this.left(i)) < 0) smaller = this.getRightIndex(i);
      if(this.comparator(this.items[i], this.items[smaller]) < 0) break;
      this.swap(i, smaller);
      i = smaller;
    }
  }

  public empty() {
    return this.size === 0;
  }
}

type LinkedList<T> = { val: T, prev?: LinkedList<T> };
export type DijkstraCon<T> = { to: T, cost: number, from: LinkedList<T> };
export const dijkstra = <T>(
  start: T, goal: T,
  getCons: (from: T) => Omit<DijkstraCon<T>, 'from'>[],
  transform: (arg: T) => any = id,
): { totalCost: number, path: LinkedList<T> } | undefined => {
  const queue = new Heap<DijkstraCon<T>>((a, b) => a.cost - b.cost);
  const minCons = new Map<any, DijkstraCon<T>>();
  const seen = new Set<any>([transform(start)]);

  const transformedGoal = transform(goal);

  for(const nextCon of getCons(start)) {
    const finalizedNextCon: DijkstraCon<T> = { ...nextCon, from: { val: start } };
    minCons.set(transform(nextCon.to), finalizedNextCon);
    queue.add(finalizedNextCon);
  }

  while(!queue.empty()){
    const con = queue.pop()!;
    const transformedTo = transform(con.to);
   
    if(transformedTo === transformedGoal){
      return {
        path: { val: con.to, prev: con.from },
        totalCost: con.cost,
      }
    }

    if(seen.has(transformedTo)) throw new Error(`does this happen? ${transformedTo}`);
    seen.add(transformedTo);
    minCons.delete(transformedTo);

    for(const nextCon of getCons(con.to)){
      const transformedNext = transform(nextCon.to);
      if(seen.has(transformedNext)) continue;
      const minConToNext = minCons.get(transformedNext);
      const finalizedNextCon: DijkstraCon<T> = { to: nextCon.to, cost: nextCon.cost + con.cost, from: { val: con.to, prev: con.from } };
      if(!minConToNext) {
        minCons.set(transformedNext, finalizedNextCon);
        queue.add(finalizedNextCon);
      }else{
        if(minConToNext.cost > finalizedNextCon.cost){
          minConToNext.cost = finalizedNextCon.cost;
          minConToNext.from = finalizedNextCon.from;
        }
      }
      
    }
  }
}

export const repeatedAp = <T>(fn: (arg: T) => T, count: number, arg: T): T => {
  for(let i = 0; i < count; i++) arg = fn(arg);
  return arg;
}
