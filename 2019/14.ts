import { readFile } from '../tools.ts';

type Chem = {
  amount: number,
  name: string,
}

const tome: Record<string, { reactants: Chem[], magnitude: number, stored: number }> = {};

const input = readFile('./inputs/14.txt');
  

//temporary array for processing in the data
const dict = input.split('\n').map(line=>{
  let half = line.split('=>').map(s=>s.split(',').map(s=>{
    let comp = s.trim().split(' ');
    return {
      amount: Number(comp[0]),//obviously the associated amount
      name: comp[1]//compound key
    }
  }));
  return {
    reactants: half[0],
    product: half[1][0]
  }
});

//loading tome
for(let rxn of dict){
  tome[rxn.product.name] = {
    reactants: rxn.reactants, //array of objects created on line 10
    magnitude: rxn.product.amount, //how much product the reaction created
    stored: 0 //used as buffer for excese resource
  };
}

const compute = (key: string, toMake: number): number => { 
  if(key=='ORE') return toMake;
  const rxn = tome[key];

  //removed excess from how much we need to make
  if(rxn.stored > toMake){
    rxn.stored -= toMake;
    return 0;
  }

  toMake -= rxn.stored;

  rxn.stored = (rxn.magnitude - (toMake % rxn.magnitude)) % rxn.magnitude; //will be how much extra this reaction will produce

  const mult = Math.ceil(toMake / rxn.magnitude); // calculate how much total ingrediants are needed

  let subTotal = 0;
  for(const cmp of rxn.reactants){
    subTotal += compute(cmp.name, cmp.amount * mult); 
  }
  return subTotal;
};

console.log("Part 1: " + compute("FUEL",1));

//binary search for answer
let low = 0;
let high = 2 ** 24;
while(low<high){
  let mid = Math.floor((high + low) / 2);
  let cur = compute("FUEL", mid);
  if(1e12 < cur){
    high = mid - 1;
  }else{ 
    low = mid;
  }
}

console.log("Part 2: " + (low-1));
