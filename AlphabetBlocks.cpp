//
//  AlphabetBlocks.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 15.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <array>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

using namespace std;

struct Instance {
  string goal;
  vector<string> blocks;
};

Instance parseInstance(const string& line) {
  istringstream stream(line);
  Instance inst;
  size_t num_blocks;
  stream >> num_blocks;
  stream.ignore(2, '|');
  stream >> inst.goal;
  stream.ignore(2, '|');

  inst.blocks.resize(num_blocks);
  for( auto& block: inst.blocks )
    stream >> block;

  return inst;
}

bool solveInstance(string::const_iterator pos, string::const_iterator end, const vector<string>& blocks, vector<bool>& used) {
  if( pos == end )
    return true;

  for( size_t i = 0; i < blocks.size(); ++i ) {
    if( !used[i] && blocks[i].find(*pos) != string::npos ) {
      used[i] = true;
      if( solveInstance(pos+1, end, blocks, used) ) {
        return true;
      }
      used[i] = false;
    }
  }

  return false;
}

bool solveInstance(const Instance& inst) {
  vector<bool> used(inst.blocks.size(), false);
  return solveInstance(inst.goal.cbegin(), inst.goal.cend(), inst.blocks, used);
}


int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      Instance inst = parseInstance(line);
      cout << (solveInstance(inst) ? "True" : "False") << endl;
    }
  }
  file.close();
}
