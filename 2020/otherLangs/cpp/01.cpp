#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

int main(){
  ifstream inputfile;
  inputfile.open("./inputs/01.txt");
  vector<int> parsed = {};
  string line;
  while(getline(inputfile, line)){
    parsed.push_back(stoi(line));
  }
  int p1;
  int p2;
  for(int i = 0; i < parsed.size(); i++){
    for(int j = i + 1; j < parsed.size(); j++){
      if(parsed[i] + parsed[j] == 2020) p1 = parsed[i] * parsed[j];
      for(int k = j + 1; k < parsed.size(); k++){
        if(parsed[i] + parsed[j] + parsed[k] == 2020) p2 = parsed[i] * parsed[j] * parsed[k];
      }
    }
  }
  cout << "Part 1: " << p1 << endl;
  cout << "Part 2: " << p2 << endl;
}
