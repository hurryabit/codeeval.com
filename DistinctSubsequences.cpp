//
//  DistinctSubsequences.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 14.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>

using namespace std;

int possibilities(const string& x, const string& y) {
  size_t m = x.size(), n = y.size();
  int table[m+1][n+1];

  for( size_t i = 1; i <= m; ++i )
    table[i][0] = 0;
  for( size_t j = 0; j <= n; ++j )
    table[0][j] = 1;

  for( size_t j = 1; j <= n; ++j ) {
    for( size_t i = 1; i <= m; ++i ) {
      if( x[i-1] == y[j-1] )
        table[i][j] = table[i-1][j-1] + table[i][j-1];
      else
        table[i][j] = table[i][j-1];
//      cout << "table[" << i << "][" << j << "] = " << table[i][j] << endl;
    }
  }
  return table[m][n];
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      size_t comma = line.find(',');
      string sequence = line.substr(0, comma), subsequence = line.substr(comma+1);
      cout << possibilities(subsequence, sequence) << endl;
    }
  }
  file.close();
}
