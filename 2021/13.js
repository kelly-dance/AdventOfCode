import { printChar, readAdvent } from '../tools.ts';

let [pairs, folds] = (await readAdvent()).split('\n\n')
pairs = pairs.split('\n').map(s => s.split(',').map(n => parseInt(n)));
folds = folds.split('\n').map(l => {
  const [w, n] = l.split(' ')[2].split('=');
  return [w, parseInt(n)];
});

const fold = (pts, w, n) => {
  const newPts = new Map();
  if(w === 'x'){
    for(const [x, y] of pts.values()){
      if(x < n) newPts.set(`${x},${y}`,[x, y]);
      else {
        const nx = 2 * n - x;
        newPts.set(`${nx},${y}`, [nx, y]);
      }
    }
  } else {
    for(const [x ,y] of pts.values()){
      if(y < n) newPts.set(`${x},${y}`, [x, y]);
      else {
        const ny = 2 * n - y;
        newPts.set(`${x},${ny}`, [x, ny]);
      }
    }
  }
  
  return newPts;
}

let pts = new Map();
for(const [x, y] of pairs){
  pts.set(`${x},${y}`, [x, y]);
}

pts = fold(pts, ...folds[0]);
console.log(pts.size);

for(const [w, n] of folds.slice(1)){
  pts = fold(pts, w, n);
}

for(let i = 0; i < 6; i++){
  for(let j = 0; j < 39; j++){
    printChar(pts.has(`${j},${i}`) ? '#' : ',');
  }
  printChar('\n');
}
