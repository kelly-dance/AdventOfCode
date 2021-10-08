import { readFile, pickIntsFromString, gcd, lcm } from '../tools.ts';

const inp = readFile('./inputs/12.txt');

let planets = inp.split('\n').map(line => pickIntsFromString(line)).map(arr=>({
  x: arr[0],
  y: arr[1],
  z: arr[2],
  vx: 0,
  vy: 0,
  vz: 0
}));

console.log(planets)

let initialState = JSON.parse(JSON.stringify(planets));
let xPeroid = 0;
let yPeroid = 0;
let zPeroid = 0;

let t = 0;
while(t <= 1000 || !xPeroid || !yPeroid || !zPeroid){
  if(t==1000){ // output part 1 solution
    console.log('Part 1: ' + planets.reduce((acc,p)=>{
      let pot = Math.abs(p.x) + Math.abs(p.y) + Math.abs(p.z); 
      let kin = Math.abs(p.vx) + Math.abs(p.vy) + Math.abs(p.vz); 
      return acc+pot*kin;
    }, 0));
  }

  // check if any planets have reached their original position
  let xValid = true;
  let yValid = true;
  let zValid = true;
  for(let i = 0; i < planets.length; i++){
    if(planets[i].x !== initialState[i].x) xValid = false;
    if(planets[i].y !== initialState[i].y) yValid = false;
    if(planets[i].z !== initialState[i].z) zValid = false;
  }
  if(!xPeroid && xValid) xPeroid = t;
  if(!yPeroid && yValid) yPeroid = t;
  if(!zPeroid && zValid) zPeroid = t;
  
  // planet simulation
  // applying acceleration
  for(let ai = 0; ai < planets.length-1; ai++){
    const a = planets[ai];
    for(let bi = ai+1; bi < planets.length; bi++){
      const b = planets[bi];

      // x
      if(a.x < b.x){
        a.vx++;
        b.vx--;
      }else if(a.x != b.x){
        a.vx--;
        b.vx++;
      }

      // y
      if(a.y < b.y){
        a.vy++;
        b.vy--;
      }else if(a.y !== b.y){
        a.vy--;
        b.vy++;
      }

      // z
      if(a.z < b.z){
        a.vz++;
        b.vz--;
      }else if(a.z !== b.z){
        a.vz--;
        b.vz++;
      }
    }
  }

  // applying velocity
  for(const c of planets){
    c.x += c.vx;
    c.y += c.vy;
    c.z += c.vz;
  }
  
  t++;
}

console.log('Part 2: ' + lcm(lcm(Math.abs(xPeroid + 1), Math.abs(yPeroid + 1)), Math.abs(zPeroid + 1)));

