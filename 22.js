import { product, range, readFile, sum, zipWith } from './tools.ts';

(()=>{ // PART 1
  const [p1, p2] = readFile('inputs/22.txt').split('\r\n\r\n').map(g => g.split('\r\n').slice(1).map(Number));

  while(p1.length * p2.length){
    const p1c = p1.shift();
    const p2c = p2.shift();
    if(p1c > p2c) p1.push(p1c, p2c)
    else p2.push(p2c, p1c)
  }
  
  const winner = p1.length ? p1 : p2;
  
  const score = sum(zipWith(product, winner.reverse(), range(1,winner.length+1)))
  
  console.log(score)
})();

(()=>{ // PART 2
  const [p1start, p2start] = readFile('inputs/22.txt').split('\r\n\r\n').map(g => g.split('\r\n').slice(1).map(Number));

  const play = (p1, p2) => {
    let history = new Set();
    while(p1.length * p2.length){
      const p1c = p1.shift();
      const p2c = p2.shift();
      const asStr = p1.join()+'_'+p2.join();
      if(history.has(asStr)) p1.push(p1c, p2c);
      else {
        history.add(asStr);
        if(p1c <= p1.length && p2c <= p2.length){
          if(play(p1.slice(0,p1c), p2.slice(0,p2c))[0]) p1.push(p1c, p2c);
          else  p2.push(p2c, p1c);
        }else{
          if(p1c > p2c) p1.push(p1c, p2c);
          else p2.push(p2c, p1c);
        }
      }
    }
    return [!!p1.length, p1.length ? p1 : p2];
  }
  const winner = play(p1start, p2start)[1];
  console.log(sum(zipWith(product, winner.reverse(), range(1,winner.length+1))))
})()

