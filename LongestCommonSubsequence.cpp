//
//  LongestCommonSubsequence.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 08.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

string lcs(const string& xs, const string& ys) {
  vector<vector<int>> table(xs.size()+1, vector<int>(ys.size()+1, 0));
  for( long i = xs.size()-1; i >= 0; --i ) {
    for( long j = ys.size()-1; j >= 0; --j ) {
      if( xs[i] == ys[j] )
        table[i][j] = table[i+1][j+1] + 1;
      else
        table[i][j] = max(table[i][j+1], table[i+1][j]);
    }
  }

  string result;
  result.reserve(table[0][0]);
  long i = 0, j = 0;
  while( table[i][j] > 0 ) {
    if( table[i][j] == table[i][j+1] )
      ++j;
    else if( table[i][j] == table[i+1][j] )
      ++i;
    else {
      result.push_back(xs[i]);
      ++i;
      ++j;
    }
  }
  return result;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      size_t sep = line.find(';');
      cout << lcs(line.substr(0, sep), line.substr(sep+1)) << endl;
    }
  }
  file.close();
}
