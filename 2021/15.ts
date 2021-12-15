import { DijkstraCon, dijkstra, range, readAdvent, Vec2 } from '../tools.ts';

const solve = (grid: number[][]) => {
  console.log(dijkstra(
    new Vec2(0, 0), // start
    new Vec2(grid.length - 1, grid[0].length - 1), // end
    p => { // function to resolve edges
      return Vec2.SIDES
        .map(o => ({ to: p.add(o), cost: p.add(o).get(grid) }))
        .filter(c => c.cost !== undefined) as DijkstraCon<Vec2>[];
    },
    v => v.toString(), // function to turn my nodes of type Vec2 into something primative (in this case string)
  )!.totalCost);
}

const inp = (await readAdvent()).split('\n').map(s =>  s.split('').map(c => parseInt(c)));
solve(inp);

const part2 = range(5).flatMap(i => inp.map(r => range(5).flatMap(j => r.map(n => (n + i + j - 1) % 9 + 1))));
solve(part2);
